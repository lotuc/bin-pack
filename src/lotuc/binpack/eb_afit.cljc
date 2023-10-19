(ns lotuc.binpack.eb-afit
  "eb stands for the paper's author Erhan Baltacioglu, and afit stands for Air
  Force Institude of Technology."
  #?(:clj (:refer-clojure :exclude [get-in assoc-in assoc]))
  (:require
   #?(:clj [clj-fast.clojure.core :refer [get-in assoc-in assoc]]))
  #?(:clj (:import (java.lang Boolean Long))))

#?(:clj (set! *unchecked-math* true))

;; assume all dimension value is lower than this: Integer/MAX_VALUE
(def MAX_SIZE 2147483647)

(def ^:private ^:dynamic *time-bound-in-millis* nil)
(def ^:private ^:dynamic *starts-at-in-millis* nil)
(def ^:private ^:dynamic *finished* nil)
(def ^:private ^:dynamic *use-pmap* nil)

#?(:clj (defn- bound-fn-* [f]
          (let [time-bound-in-millis *time-bound-in-millis*
                starts-at-in-millis *starts-at-in-millis*
                finished *finished*]
            (fn [& args]
              (binding [*time-bound-in-millis* time-bound-in-millis
                        *starts-at-in-millis* starts-at-in-millis
                        *finished* finished]
                (apply f args))))))

(declare exec-iterations)
(declare exec-iteration-on-pallet-variant)
(declare exec-iteration-on-pallet-variant-with-layer-thickness)

;;; 1. `exec-iterations` generates pallet rotations and run each one with
;;;    `exec-iteration-on-pallet-variant`
;;; 2. `exec-iteration-on-pallet-variant` iterate on every possible first
;;;     layer's thickness (which are ordered by its weight) using
;;;     `exec-iteration-on-pallet-variant-with-layer-thickness`.
;;; 3. `exec-iteration-on-pallet-variant-with-layer-thickness` do layer by layer
;;;    packing
;;;    - choose the layer's thickness (first layer's thickness is given by its
;;;      parameter)
;;;    - packing the layer as many as boxes into pallet within layer
;;;      - do a `layer-in-layer` packing if neccessary

(declare build-iteration-input)
(declare build-unpacked)

(defn find-best-pack
  "Find the best packing strategy for given input using the algorithm described in
  paper https://scholar.afit.edu/etd/4563/.

  The reference implemention described in the paper can be found here at
  https://github.com/wknechtel/3d-bin-pack.

  The implementation is a modified version.

  Notice that we assume all numbers in patermeter to be long.

  Parameter
  - `input` `:pallet-dims`: a vector of the pallet's 3 dimensions [px py pz]
  - `input` `:boxes`: a vector of box types, each of its item is
      - dims: a vector of the box's 3 dimensions [dim1 dim2 dim3]
      - n: represents the count of box with such a dims.

  Optional Parameter
  - `time-bound-in-millis`: if given, will track elapsed time since begining,
    throw error on exceeding the time bound.
  - `use-pmap`: defaults to be `false`; if `true`, use `pamp` for the iteration
     on `exec-iteration-on-pallet-variant` (only works on Clojure).

  Returns the best packing results (returns nil if no packing scheme found)
  - packed-volume: sum volume of the packed boxes
  - packed-number: number of boxes packed
  - percentage-used: percentage of packed-volume in pallet volume
  - pallet-variant: pallet orientation varint
  - first-layer-thickness
  - pack: the packed boxes ordered by packing order, the item is taken from the
    input box, except the following fields
    - pack-dims: packing orientation
    - pack-coord: coordination of the packed box
  - unpacked: unpacked boxes list, the item is taken from the input box, except
    the following field
    - unpacked-n: the unpacked count of the box type
  ```
  (find-best-pack {:pallet-dims [10 5 5],
                   :boxes [{:dims [5 5 5], :n 3}]})
  ;; ->
  {:pallet-volume 250,
   :box-volume 375,
   :packed-volume 250,
   :packed-number 2,
   :pallet-variant [10 5 5],
   :percentage-used 100.0,
   :first-layer-thickness 5,
   :pack
   [{:pack-dims [5 5 5], :pack-coord [0 0 0], :dims [5 5 5], :vol 125, :n 3}
    {:pack-dims [5 5 5], :pack-coord [5 0 0], :dims [5 5 5], :vol 125, :n 3}],
   :unpacked {:dims [5 5 5], :vol 125, :n 3, :unpacked-n 1}}
  ```"
  [input & {:keys [time-bound-in-millis use-pmap]}]
  (let [use-pmap (if (some? use-pmap) use-pmap false)
        {:keys [pallet-volume box-volume] :as iteration-input} (build-iteration-input input)
        data (binding [*time-bound-in-millis* time-bound-in-millis
                       *starts-at-in-millis* #?(:clj (. System (currentTimeMillis))
                                                :cljs (. js/Date (now)))
                       *finished* (atom false)
                       *use-pmap* use-pmap]
               (exec-iterations iteration-input))]
    (when-some [{:keys [best-pack best-layer-thickness best-volume
                        best-packed-number best-pallet-variant percentage-used]}
                data]
      (let [pack (let [!pack (atom [])]
                     ;; exec the best packing iteration for the packing info
                     ;; with packing order
                   (exec-iteration-on-pallet-variant-with-layer-thickness
                    best-pallet-variant best-layer-thickness iteration-input :on-pack #(swap! !pack conj %))
                   @!pack)
            unpacked (build-unpacked best-pack)]
        {:packed-volume best-volume
         :packed-number best-packed-number
         :pallet-variant best-pallet-variant
         :percentage-used percentage-used
         :first-layer-thickness best-layer-thickness
         :pack pack
         :unpacked unpacked
         :pallet-volume pallet-volume
         :box-volume box-volume}))))

(defn pack-boxes
  "Generate the packing result for given parameters.

  The eb-afit's packing result is determined by two simple parameters:
  - `pallet-variant`: the pallet's rotation variant
  - `first-layer-thickness`: as the name indicates"
  [input {:keys [pallet-variant first-layer-thickness]}]
  (let [{:keys [pallet-volume box-volume] :as iteration-input}
        (build-iteration-input input)

        !pack (atom [])

        {:keys [boxes] :as r}
        (exec-iteration-on-pallet-variant-with-layer-thickness
         pallet-variant first-layer-thickness iteration-input
         :on-pack #(swap! !pack conj %))

        pack @!pack
        unpacked (build-unpacked boxes)
        packed-boxes (->> boxes (filter :pack-dims))
        packed-volume (->> packed-boxes (map :vol) (reduce + 0))
        percentage-used (/ (* 100.0 packed-volume) pallet-volume)]
    {:packed-volume packed-volume
     :packed-number (->> packed-boxes (map (constantly 1)) (reduce + 0))
     :pallet-variant pallet-variant
     :percentage-used percentage-used
     :first-layer-thickness first-layer-thickness
     :pack pack
     :unpacked unpacked
     :pallet-volume pallet-volume
     :box-volume box-volume}))

(defn- check-finished []
  (when (and *finished* @*finished*)
    (throw (ex-info "short circuit exception on finished" {:reason :finished}))))

(defn- build-unpacked [boxes]
  (let [tbn (count boxes)]
    (loop [i 0 unpacked []]
      (if (< i tbn)
        (let [{:keys [n] :as b} (boxes i)
              unpacked-n (->> (range i (+ i n))
                              (filter (comp (complement :pack-dims) boxes))
                              count)]
          (recur (+ i n)
                 (cond-> unpacked
                   (not (zero? unpacked-n))
                   (conj (-> b
                             (assoc :unpacked-n unpacked-n)
                             (dissoc :pack-dims :pack-coord))))))
        unpacked))))

(defn make-rotation
  ([[x y z] variant]
   (case variant
     0 [x y z] 1 [z y x] 2 [z x y] 3 [y x z] 4 [x z y] 5 [y z x])))

(defn rotate-dim1 [[dim1 dim2 dim3]]
  [[dim1 dim2 dim3] [dim2 dim1 dim3] [dim3 dim1 dim2]])

(defn cube? [[dim1 dim2 dim3]]
  (and (= dim1 dim2) (= dim2 dim3)))

(defn vectorize-box-coords [boxes]
  (let [box-xs #?(:clj (make-array Long/TYPE (count boxes))
                  :default (make-array (count boxes)))
        box-ys #?(:clj (make-array Long/TYPE (count boxes))
                  :default (make-array (count boxes)))
        box-zs #?(:clj (make-array Long/TYPE (count boxes))
                  :default (make-array (count boxes)))]
    (doseq [[i {[x y z] :dims}]
            (map-indexed (fn [i b] [i b]) boxes)]
      #?(:clj (aset ^longs box-xs i x)
         :default (aset box-xs i x))
      #?(:clj (aset ^longs box-ys i y)
         :default (aset box-ys i y))
      #?(:clj (aset ^longs box-zs i z)
         :default (aset box-zs i z)))
    {:box-xs box-xs
     :box-ys box-ys
     :box-zs box-zs}))

(defn build-iteration-input
  [{:keys [boxes pallet-dims] :as input}]
  (let [ceil-long (fn [v] (let [v' (long v)] (if (< v' v) (inc v') v')))
        boxes (->> boxes
                   (map (fn [{:keys [dims n] :as b}]
                          (let [dims (->> dims (map ceil-long) (into []))
                                b' (assoc b
                                          :dims dims
                                          :n (ceil-long n)
                                          :vol (apply * dims))]
                            (map (fn [_] b') (range n)))))
                   flatten
                   (into []))
        pallet-dims (->> pallet-dims (map ceil-long) (into []))
        box-volume (reduce + (map :vol boxes))
        pallet-volume (apply * pallet-dims)]
    (-> input
        (assoc :boxes boxes
               :pallet-dims pallet-dims
               :box-volume box-volume
               :pallet-volume pallet-volume)
        (merge (vectorize-box-coords boxes)))))

(defn calc-layer-weight
  [{:keys [boxes box-xs box-ys box-zs box-packed]} ^long x ^long exdim]
  (let [tbn (count boxes)]
    (loop [z 0 r 0]
      (if (< z tbn)
        (if (or (= x z) (aget ^booleans box-packed z))
          (recur (unchecked-inc z) r)
          (let [dx #?(:clj (aget ^longs box-xs z) :default (aget box-xs z))
                dy #?(:clj (aget ^longs box-ys z) :default (aget box-ys z))
                dz #?(:clj (aget ^longs box-zs z) :default (aget box-zs z))]
            (recur (unchecked-inc z)
                   (unchecked-add
                    r
                    (min (abs (unchecked-subtract exdim dx))
                         (abs (unchecked-subtract exdim dy))
                         (abs (unchecked-subtract exdim dz)))))))
        r))))

(defn list-candit-layer-thickness-with-weight
  "LISTS ALL POSSIBLE LAYER HEIGHTS BY GIVING A WEIGHT VALUE TO EACH OF THEM."
  [{:keys [boxes] :as iteration-input} [px py pz :as pallet]]
  (let [s (-> iteration-input
              (select-keys [:box-xs :box-ys :box-zs])
              (assoc :boxes boxes
                     :box-packed #?(:clj (make-array Boolean/TYPE (count boxes))
                                    :default (make-array (count boxes)))))]
    (->> (for [x (range (count boxes))

               [exdim dimen2 dimen3]
               (rotate-dim1 (:dims (boxes x)))

               :when
               (and (<= exdim py)
                    (or (and (<= dimen2 px) (<= dimen3 pz))
                        (and (<= dimen3 px) (<= dimen2 pz))))]
           [exdim (delay {:weight (calc-layer-weight s x exdim) :dim exdim})])
         (into {})
         (map (comp deref second)))))

(defn find-smallest-z
  "Array of {:keys [cumz cumx]}"
  [scrap-pad]
  (case (count scrap-pad)
    0 nil
    1 0
    (loop [r 0
           r-cumz (:cumz (scrap-pad 0))
           i 1
           [{i-cumz :cumz :as i-scrap-pad} & xs] (rest scrap-pad)]
      (if (nil? i-scrap-pad)
        r
        (if (< i-cumz r-cumz)
          (recur i i-cumz (unchecked-inc i) xs)
          (recur r r-cumz (unchecked-inc i) xs))))))

(defn analyze-box
  "ANALYZES EACH UNPACKED BOX TO FIND THE BEST FITTING ONE TO THE EMPTY SPACE
  GIVEN

  Figure 3-6: Findbox Function parameters.

  - gap-geo: the gap geometry paraemters
  - box: the box analyzing
  - best-fits: best fits up to current
      - bf: the box fits in the current layer
      - b-bf: the box exceeds the current layer
  "
  [{:keys [hmx hy hmy hz hmz] :as gap-geo}
   {:keys [bf b-bf]
    [bfx   bfy   bfz] :box-fits
    [b-bfx b-bfy b-bfz] :b-box-fits
    :as best-fits}
   {:keys [dims index]
    [dim1 dim2 dim3] :dims
    :as box}]
  (merge
   best-fits
   (when (and (<= dim1 hmx) (<= dim2 hmy) (<= dim3 hmz))
     (let [bfy' (abs (- hy dim2))]
       (if (<= dim2 hy)
         (when (or (< bfy' bfy)
                   (and (= bfy' bfy) (< (- hmx dim1) bfx))
                   (and (= bfy' bfy) (= (- hmx dim1) bfx) (< (abs (- hz dim3)) bfz)))
           {:box box :box-fits [(- hmx dim1) bfy' (abs (- hz dim3))]})
         (when (or (< bfy' b-bfy)
                   (and (= bfy' b-bfy) (< (- hmx dim1) b-bfx))
                   (and (= bfy' b-bfy) (= (- hmx dim1) b-bfx) (< (abs (- hz dim3)) b-bfz)))
           {:b-box box :b-box-fits [(- hmx dim1) bfy' (abs (- hz dim3))]}))))))

(defn analyze-box-with-rotation
  [gap-geo best-fits {:keys [dims] :as box}]
  (->> (range (if (cube? dims) 1 6))
       (map #(update box :dims make-rotation %))
       (reduce (partial analyze-box gap-geo) best-fits)))

(defn find-box
  "FINDS THE MOST PROPER BOXES BY LOOKING AT ALL SIX POSSIBLE ORIENTATIONS, EMPTY
  SPACE GIVEN, ADJACENT BOXES, AND PALLET LIMITS"
  [{:keys [hmx hy hmy hz hmz] :as smallest-z-gap-geo}
   {:keys [boxes box-packed] :as state}]
  (let [tbn (count boxes)

        init-best-fits
        {:box nil
         :box-fits [MAX_SIZE MAX_SIZE MAX_SIZE]

         :b-box nil
         :b-box-fits [MAX_SIZE MAX_SIZE MAX_SIZE]}

        ;; find the packed box or the last one for given type which starts at
        ;; index and has n boxes.
        try-find-unpacked-box
        (fn [index n]
          (let [maxn (+ index n)]
            (loop [i index]
              (if (< i maxn)
                (if (aget ^booleans box-packed i)
                  (recur (unchecked-inc i))
                  (assoc (boxes i) :index i))
                (let [i (dec i)]
                  (assoc (boxes i) :index i))))))

        ;; first box of the next box type.
        next-box-type
        (fn [y] (+ y (:n (boxes y))))]

    (loop [i 0 best-fits init-best-fits]
      (let [{:keys [pack-dims] :as b}
            (when (< i tbn) (try-find-unpacked-box i (:n (boxes i))))]
        (if (or (nil? b) (>= i tbn))
          best-fits
          (let [best-fits' (if pack-dims best-fits
                               (analyze-box-with-rotation
                                smallest-z-gap-geo best-fits b))]
            (recur (next-box-type i) best-fits')))))))

(defn apply-found-box-to-pack
  [{:keys [packed-y boxes box-packed smallest-z scrap-pad on-pack]
    :as state}
   {:keys [^long depth-left ^long depth-right]
    {:keys [^long index dims]} :found-box}
   {:keys [box-volume pallet-volume] :as input}]
  (let [box-vol
        (get-in boxes [index :vol])

        cox
        (cond
          (= (count scrap-pad) 1) 0

          ;; attach to right
          (> depth-right depth-left)
          (let [cumx (:cumx (scrap-pad smallest-z))
                dimx (dims 0)]
            (- cumx dimx))

          ;; attach to left
          :else
          (:cumx (scrap-pad (dec smallest-z))))
        coz (get-in scrap-pad [smallest-z :cumz])
        coy packed-y

        {:keys [packed-volume] :as state'}
        (-> state
            (assoc-in [:boxes index :pack-dims] dims)
            (assoc-in [:boxes index :pack-coord] [cox coy coz])
            (update :packed-volume #(+ (or % 0) box-vol))
            (update :packed-number #(inc (or % 0))))]

    (when on-pack
      (on-pack (merge {:pack-dims dims :pack-coord [cox coy coz]}
                      (boxes index))))

    #?(:clj (aset ^booleans box-packed index true)
       :default (aset box-packed index true))

    (cond-> state'
      (or (= packed-volume box-volume) (= packed-volume pallet-volume))
      (assoc :packing false :hundred-percent? true))))

(defn apply-found-box-to-scrap-pad
  [{:keys [smallest-z scrap-pad] :as state}
   {:keys [scrap-pad-left scrap-pad-right
           ^long depth-left ^long depth-right ^boolean gap-filled?]
    {:keys [index] [dimx _ dimz] :dims} :found-box}]
  (let [{:keys [cumx cumz] :as smallest-z-gap} (scrap-pad smallest-z)
        {pre-cumx :cumx} (last scrap-pad-left)]
    (if (> depth-right depth-left)

      ;; new box attach to right
      (into
       ;; left side of the new box (the left top point)
       (if gap-filled?
         (if (= dimz depth-left)
           (into [] (drop-last scrap-pad-left))
           scrap-pad-left)
         (conj scrap-pad-left {:cumx (- cumx dimx) :cumz cumz}))

       ;; right side of the new box (the right top point)
       (if (= dimz depth-right)
         scrap-pad-right
         (into [{:cumx cumx :cumz (+ cumz dimz)}]
               scrap-pad-right)))

      ;; else new box attach to left
      (into
       ;; left side of the new box
       (if (= dimz depth-left)
         (into [] (drop-last scrap-pad-left))
         scrap-pad-left)
       ;; right side of the new box
       (if gap-filled?
         ;; the box is evening the gap
         (if (= dimz depth-right)
           scrap-pad-right
           (into [{:cumx cumx :cumz (+ cumz dimz)}] scrap-pad-right))
         (into [{:cumx (+ (or pre-cumx 0) dimx)
                 :cumz (+ cumz dimz)} smallest-z-gap]
               scrap-pad-right))))))

(defn apply-found-box
  [{:keys [smallest-z scrap-pad] :as state}
   {:keys [index] [dimx _ _] :dims :as found-box}
   {:keys [box-volume pallet-volume] :as input}]
  (let [{:keys [cumx cumz] :as smallest-z-gap} (scrap-pad smallest-z)

        scrap-pad-left (subvec scrap-pad 0 smallest-z)
        scrap-pad-right (subvec scrap-pad (inc smallest-z))

        {pre-cumx :cumx pre-cumz :cumz :as pre} (last scrap-pad-left)
        {pos-cumz :cumz :as pos} (first scrap-pad-right)

        depth-left (if pre (- pre-cumz cumz) 0)
        depth-right (if pos (- pos-cumz cumz) 0)
        gap-filled? (= (+ (or pre-cumx 0) dimx) cumx)

        x {:scrap-pad-left scrap-pad-left
           :scrap-pad-right scrap-pad-right
           :depth-left depth-left
           :depth-right depth-right
           :gap-filled? gap-filled?
           :found-box found-box}

        scrap-pad'
        (apply-found-box-to-scrap-pad state x)]

    (-> (apply-found-box-to-pack state x input)
        (assoc :scrap-pad scrap-pad'))))

(defn apply-ignore-gap
  "return a new scrap-pad with the smallest-z gap evened."
  [{:keys [smallest-z scrap-pad] :as state}]
  (cond
    (= 1 (count scrap-pad))
    scrap-pad

    (zero? smallest-z)
    (into [] (rest scrap-pad))

    (= smallest-z (dec (count scrap-pad)))
    (let [{:keys [cumx]} (last scrap-pad)
          scrap-pad' (drop-last scrap-pad)
          {:keys [cumz]} (last scrap-pad')]
      (conj (into [] (drop-last scrap-pad'))
            {:cumx cumx :cumz cumz}))

    :else
    (let [scrap-pad-before (subvec scrap-pad 0 smallest-z)
          scrap-pad-after (subvec scrap-pad (inc smallest-z))

          {:keys [cumx]} (scrap-pad smallest-z)
          {pre-cumz :cumz :as pre} (last scrap-pad-before)
          {pos-cumz :cumz :as pos} (first scrap-pad-after)]
      (cond
        (= pre-cumz pos-cumz)
        (into (into [] (drop-last scrap-pad-before))
              scrap-pad-after)

        (< pre-cumz pos-cumz)
        (into (conj (into [] (drop-last scrap-pad-before))
                    {:cumx cumx :cumz pre-cumz})
              scrap-pad-after)

        :else
        (into scrap-pad-before scrap-pad-after)))))

(defn check-found
  "returns [layer-in-layer-updates, {layer-done? ignore-gap? c-box}]."
  [{:keys [box b-box]
    :as find-box-res}
   {:keys [layer-thickness smallest-z scrap-pad
           layer-in-layer pre-layer layer-in-layer-z]
    :as state}]
  (cond
    ;; found a box and its thickness fits in current layer.
    box
    [nil {:c-box box}]

    ;; found a box which is higher than current thickness
    ;; 1. if we are already in layer-in-layer mode, update lil params
    ;; 2. if the gaps are evened, we can accept the box and entering lil mode
    (and b-box (or layer-in-layer (= 1 (count scrap-pad))))
    (let [b-box-y (get-in b-box [:dims 1])
          pre-layer (if layer-in-layer pre-layer layer-thickness)
          layer-in-layer-z (if layer-in-layer layer-in-layer-z
                               (get-in scrap-pad [smallest-z :cumz]))
          layer-in-layer (- (+ (or layer-in-layer 0) b-box-y) layer-thickness)]
      [{:layer-in-layer layer-in-layer
        :layer-thickness b-box-y
        :pre-layer pre-layer
        :layer-in-layer-z layer-in-layer-z}
       {:c-box b-box}])

    ;; notice the condition indicates (nil? b-box), meaning no box found (even
    ;; the one exceeds current layer thickness), meaning that the layer is done.
    (= 1 (count scrap-pad))
    [nil {:layer-done? true}]

    ;; gap is not evened, but we can only find box which exceeds current layer
    ;; thickness, just ignore (even) the gap
    :else
    [nil {:ignore-gap? true}]))

(defn get-smallest-z-gap-geo
  [{:keys [scrap-pad boxes layer-thickness remain-pz remain-py smallest-z]}]
  (cond
    (= 1 (count scrap-pad))
    ;; SITUATION-1: NO BOXES ON THE RIGHT AND LEFT SIDES
    (let [{:keys [cumx cumz]} (scrap-pad smallest-z)
          hz (- remain-pz cumz)]
      {:hmx cumx :hy layer-thickness :hmy remain-py :hz hz :hmz hz})

    (zero? smallest-z)
    ;; SITUATION-2: NO BOXES ON THE LEFT SIDE
    (let [{:keys [cumx cumz]} (scrap-pad smallest-z)]
      {:hmx cumx
       :hy layer-thickness
       :hmy remain-py
       :hz (- (get-in scrap-pad [(inc smallest-z) :cumz]) cumz)
       :hmz (- remain-pz cumz)})

    (= smallest-z (dec (count scrap-pad)))
    ;; SITUATION-3: NO BOXES ON THE RIGHT SIDE
    (let [{:keys [cumx cumz]}
          (scrap-pad smallest-z)

          {pre-cumx :cumx pre-cumz :cumz}
          (scrap-pad (dec smallest-z))]
      {:hmx (- cumx pre-cumx)
       :hy layer-thickness
       :hmy remain-py
       :hz (- pre-cumz cumz)
       :hmz (- remain-pz cumz)})

    :else
    ;; SITUATION-4: THERE ARE BOXES ON BOTH OF THE SIDES
    (let [{:keys [cumx cumz]}
          (scrap-pad smallest-z)
          {pre-cumx :cumx pre-cumz :cumz}
          (scrap-pad (dec smallest-z))]
      {:hmx (- cumx pre-cumx)
       :hy layer-thickness
       :hmy remain-py
       :hz (- pre-cumz cumz)
       :hmz (- remain-pz cumz)})))

(defn pack-layer
  "PACKS THE BOXES FOUND AND ARRANGES ALL VARIABLES AND RECORDS PROPERLY"
  [{[px _py _pz] :pallet :as state} input]
  (check-finished)

  (let [init-state (assoc state :scrap-pad [{:cumx px :cumz 0}])]
    (loop [{:keys [scrap-pad] :as state} init-state]
      (let [smallest-z (find-smallest-z scrap-pad)
            state (assoc state :smallest-z smallest-z)

            found (-> (get-smallest-z-gap-geo state)
                      (find-box state))

            [layer-in-layer-state {:keys [layer-done? ignore-gap? c-box] :as r}]
            (check-found found state)

            state' (merge state layer-in-layer-state)]
        (cond
          c-box (recur (apply-found-box state' c-box input))
          ignore-gap? (recur (assoc state' :scrap-pad (apply-ignore-gap state')))
          layer-done? state'
          :else (throw (ex-info "illegal state" r)))))))

(defn find-layer-thickness
  [{:keys [boxes box-packed remain-py]
    [^long px _py ^long pz] :pallet
    :as state}]
  (let [[_ layer-thickness]
        (->> (for [^long x (range (count boxes))
                   [^long exdim ^long dim2 ^long dim3] (rotate-dim1 (:dims (boxes x)))
                   :when (and (not (:pack-dims (boxes x)))
                              (<= exdim remain-py)
                              (or (and (<= dim2 px) (< dim3 pz))
                                  (and (<= dim3 px) (< dim2 pz))))
                   :let [layer-weight (calc-layer-weight state x exdim)]]
               [layer-weight exdim])
             (reduce (fn [[weight exdim] [new-weight new-exdim]]
                       (if (< new-weight weight)
                         [new-weight new-exdim]
                         [weight exdim]))
                     [1000000 0]))]
    (when (and (not= layer-thickness 0) (<= layer-thickness remain-py))
      layer-thickness)))

(defn exec-iterations
  [{:keys [boxes box-volume pallet-dims pallet-volume] :as iteration-input}]
  (let [run-variant* (fn [pallet-variant]
                       (try (exec-iteration-on-pallet-variant
                             pallet-variant iteration-input)
                            (catch #?(:clj Exception :cljs :default) e
                              (if (= ::finish (:reason (ex-data e)))
                                nil
                                (throw e)))))
        run-variant* #?(:clj (if *use-pmap* (bound-fn-* run-variant*) run-variant*)
                        :cljs run-variant*)
        map-fn #?(:clj (if *use-pmap* pmap map) :cljs map)
        res (->> (range (if (cube? pallet-dims) 1 6))
                 (map (partial make-rotation pallet-dims))
                 (map-fn run-variant*)
                 (reduce (fn [res v]
                           (if (:hundred-percent? res)
                             res
                             (let [{:keys [hundred-percent? best-volume]} v]
                               ;; short path
                               (when hundred-percent?
                                 (reset! *finished* true))
                               (cond
                                 hundred-percent? v
                                 (nil? res) v
                                 (or (nil? v) (zero? best-volume)) res
                                 (> best-volume (:best-volume res)) v
                                 :else res))))
                         nil))]
    (reset! *finished* true)
    res))

(defn exec-iteration-on-pallet-variant
  [{:keys [px py pz] :as pallet-variant}
   {:keys [pallet-volume] :as iteration-input}]
  (when-some [sorted-layer-thickness (->> (list-candit-layer-thickness-with-weight
                                           iteration-input pallet-variant)
                                          (sort-by :weight)
                                          (map :dim)
                                          seq)]
    (loop [[layer-thickness & rest-layer-thicknesses] sorted-layer-thickness
           {:keys [best-volume] :as state} {:best-volume 0}]
      (if layer-thickness
        (let [{:keys [packed-volume packed-number boxes hundred-percent?]}
              (exec-iteration-on-pallet-variant-with-layer-thickness
               pallet-variant layer-thickness iteration-input)

              state' (if (> packed-volume best-volume)
                       {:best-volume packed-volume
                        :best-pallet-variant pallet-variant
                        :best-layer-thickness layer-thickness
                        :best-packed-number packed-number
                        :percentage-used (/ (* 100.0 packed-volume) pallet-volume)
                        :best-pack boxes
                        :input iteration-input}
                       state)]
          (if hundred-percent?
            (assoc state' :hundred-percent? true)
            (recur rest-layer-thicknesses state')))
        state))))

(defn exec-iteration-on-pallet-variant-with-layer-thickness
  [pallet-variant
   layer-thickness
   {:keys [boxes box-xs box-volume pallet-volume] :as iteration-input}
   & {:keys [on-pack]}]
  (let [[_px py pz] pallet-variant]
    (loop [state (-> iteration-input
                     (select-keys [:boxes :box-xs :box-ys :box-zs])
                     (assoc :layer-thickness layer-thickness
                            :packed-y 0
                            :remain-pz pz
                            :remain-py py
                            :layer-in-layer nil
                            :pallet pallet-variant
                            :box-packed #?(:clj (make-array Boolean/TYPE (count boxes))
                                           :default (make-array (count boxes)))
                            :on-pack on-pack))]
      (let [{:keys [packed-y layer-thickness layer-in-layer pre-layer layer-in-layer-z]
             :as packed-state}
            (pack-layer state iteration-input)

            packed-state-after-layer-in-layer
            (-> packed-state
                (as-> $ (if layer-in-layer
                          ;; do layer-in-layer packing
                          (-> $
                              (merge {:remain-py (- layer-thickness pre-layer)
                                      :packed-y (+ pre-layer packed-y)
                                      :remain-pz layer-in-layer-z
                                      :layer-thickness layer-in-layer})
                              (pack-layer iteration-input)
                              (dissoc :layer-in-layer))
                          $))
                (merge (let [packed-y' (+ packed-y layer-thickness)
                             remain-py' (- py packed-y')]
                         {:packed-y packed-y'
                          :remain-py remain-py'
                          :remain-pz pz})))

            next-layer-thickness
            (find-layer-thickness packed-state-after-layer-in-layer)]

        (if next-layer-thickness
          (recur (-> packed-state-after-layer-in-layer
                     (assoc :layer-thickness next-layer-thickness)))
          packed-state-after-layer-in-layer)))))
