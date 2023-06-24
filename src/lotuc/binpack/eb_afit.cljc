(ns lotuc.binpack.eb-afit
  "eb stands for the paper's author Erhan Baltacioglu, and afit stands for Air
  Force Institude of Technology."
  #?(:clj (:refer-clojure
           :exclude [get-in assoc-in assoc]))
  (:require
   #?(:clj [clj-fast.clojure.core :refer [get-in assoc-in assoc]])
   [clojure.set :as set])
  #?(:clj (:import
           [java.lang Boolean Long])))

#?(:clj (set! *unchecked-math* true))

;; assume all dimension value is lower thant this
;; Integer/MAX_VALUE
(def MAX_SIZE 2147483647)

(declare init-optimization-fields)
(declare exec-iterations)
(declare exec-iteration-with-pallet-and-layer-thickness)

(defn find-best-pack
  "Find the best packing strategy for given input using the algorithm described in
  paper https://scholar.afit.edu/etd/4563/.

  The reference implemention described in the paper can be found here at
  https://github.com/wknechtel/3d-bin-pack.

  The implementation is a modified version.

  Parameter
  - pallet-dims: a vector of the pallet's 3 dimensions [px py pz]
  - pallet-volume: the pallet volume (* px py pz)
  - boxes: a vector of boxes, each of its item is
      - dims: a vector of the box's 3 dimensions [dim1 dim2 dim3]
      - vol: volume of the box (* dim1 dim2 dim3)
      - n: represents the count of box with such a dims.
  - box-volume: the sum of all boxes' vol.

  Returns the best packing results
  - packed-volume: sum volume of the packed boxes
  - packed-number: number of boxes packed
  - pallet-variant: pallet orientation varint
  - first-layer-thickness
  - pack: the packed result, it's the input.boxes with pack result fields (for
    the boxes that are packed)
  ```
  (find-best-pack {:pallet-volume 1000,
                   :pallet-dims [10 10 10],
                   :boxes [{:dims [10 10 10], :vol 1000, :n 1}],
                   :box-volume 1000})
  ;; ->
  {:percentage-used 100,
   :packed-volume 1000,
   :packed-number 1,
   :pallet-variant [10 10 10],
   :first-layer-thickness 10,
   :pack
   [{:dims [10 10 10],
     :vol 1000,
     :n 1,
     :pack-dims [10 10 10],
     :pack-coord [0 0 0]}]}
  ```"
  [input]
  (let [input (init-optimization-fields input)

        {:keys [best-pack best-layer best-pallet-variant] :as r}
        (exec-iterations input)

        {pack :packing-order-boxes}
        (exec-iteration-with-pallet-and-layer-thickness
         best-pallet-variant best-layer input {:packing-order-boxes []})

        unpacked
        (->> best-pack (filter (complement :pack-dims)) (into []))]
    (-> r
        (set/rename-keys
         {:best-volume :packed-volume
          :best-packed-number :packed-number
          :best-pallet-variant :pallet-variant})
        (select-keys [:packed-volume :packed-number :pallet-variant
                      :percentage-used])
        (assoc :first-layer-thickness (get-in r [:best-layer :dim])
               :pack pack
               :unpacked unpacked))))

(defn make-rotation
  ([[x y z] variant]
   (case variant
     0 [x y z] 1 [z y x] 2 [z x y] 3 [y x z] 4 [x z y] 5 [y z x])))

(defn rotate-dim1 [[dim1 dim2 dim3]]
  [[dim1 dim2 dim3] [dim2 dim1 dim3] [dim3 dim1 dim2]])

(defn cube? [[dim1 dim2 dim3]]
  (and (= dim1 dim2) (= dim2 dim3)))

(defn make-box-xyz-arrays [_boxes]
  (let [box-xs #?(:clj (make-array Long/TYPE (count _boxes))
                  :default (make-array (count _boxes)))
        box-ys #?(:clj (make-array Long/TYPE (count _boxes))
                  :default (make-array (count _boxes)))
        box-zs #?(:clj (make-array Long/TYPE (count _boxes))
                  :default (make-array (count _boxes)))]
    (doseq [[i {[x y z] :dims}]
            (map-indexed (fn [i b] [i b]) _boxes)]
      #?(:clj (aset ^longs box-xs i x)
         :default (aset box-xs i x))
      #?(:clj (aset ^longs box-ys i y)
         :default (aset box-ys i y))
      #?(:clj (aset ^longs box-zs i z)
         :default (aset box-zs i z)))

    {:box-xs box-xs
     :box-ys box-ys
     :box-zs box-zs}))

(defn init-optimization-fields
  [{boxes :boxes :as input}]
  (merge input (make-box-xyz-arrays boxes)))

(defn inject-optimization-fields-to-state
  [state {:keys [boxes box-xs box-volume pallet-volume] :as input}]
  (cond-> state

    true
    (assoc :box-packed #?(:clj (make-array Boolean/TYPE (count boxes))
                          :default (make-array (count boxes))))

    ;; reuse the box xyz arrays if it's prepared in the input
    box-xs
    (merge (select-keys input [:box-xs :box-ys :box-zs]))
    (not box-xs)
    (merge (make-box-xyz-arrays boxes))))

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

(defn list-candit-layers
  "LISTS ALL POSSIBLE LAYER HEIGHTS BY GIVING A WEIGHT VALUE TO EACH OF THEM."
  [{:keys [boxes box-xs] :as input} [px py pz]]
  (let [s (inject-optimization-fields-to-state {:boxes boxes} input)]
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

(defn sort-layers [layers]
  (sort-by :weight layers))

(defn find-smallest-z
  "Array of {:keys [cumz cumx]}"
  [scrap-pad]
  (case (count scrap-pad)
    0 nil
    1 0
    (loop [r 0 r-cumz (:cumz (scrap-pad 0))

           i 1 [{i-cumz :cumz :as i-scrap-pad} & xs]
           (rest scrap-pad)]
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
  [{:keys [packed-y boxes box-packed smallest-z scrap-pad
           packing-order-boxes]
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
            (update :packed-number #(inc (or % 0))))

        state'
        (cond-> state'
          packing-order-boxes
          (update :packing-order-boxes conj
                  (merge {:pack-dims dims :pack-coord [cox coy coz]}
                         (boxes index))))]

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
          hz  (- remain-pz cumz)]
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
  (let [init-state (assoc state :scrap-pad [{:cumx px :cumz 0}])]
    (loop [{:keys [scrap-pad boxes] :as state} init-state]
      (let [smallest-z (find-smallest-z scrap-pad)
            state (assoc state :smallest-z smallest-z)

            found (-> (get-smallest-z-gap-geo state)
                      (find-box state))

            [layer-in-layer-state {:keys [layer-done? ignore-gap? c-box] :as r}]
            (check-found found state)

            state' (merge state layer-in-layer-state)]
        (cond
          c-box
          (recur (apply-found-box state' c-box input))
          ignore-gap?
          (recur (assoc state' :scrap-pad (apply-ignore-gap state')))

          layer-done? state'
          :else (throw (ex-info "illegal state" r)))))))

(defn find-layer
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
    (if (or (= layer-thickness 0) (> layer-thickness remain-py))
      {:packing false}
      {:packing true :layer-thickness layer-thickness})))

(declare exec-iteration-on-pallet-variant)
(declare exec-iteration-with-pallet-and-layer-thickness)

(defn exec-iteration-on-pallet-variant
  [{:as pallet-variant-state}
   {:keys [px py pz] :as pallet-variant}
   {:keys [pallet-volume] :as input}]
  (loop [[layer & rest-layers]
         (-> (list-candit-layers input pallet-variant) sort-layers)

         {:keys [best-volume] :as state}
         pallet-variant-state]
    (if layer
      (let [{:keys [packed-volume packed-number boxes hundred-percent?] :as s}
            (exec-iteration-with-pallet-and-layer-thickness
             pallet-variant layer input)

            state'
            (if (> packed-volume best-volume)
              {:best-volume packed-volume
               :best-pallet-variant pallet-variant
               :best-layer layer
               :best-packed-number packed-number
               :percentage-used (/ (* 100.0 packed-volume) pallet-volume)
               :best-pack boxes
               :input input}
              state)]
        (if hundred-percent?
          (assoc state' :hundred-percent? true)
          (recur rest-layers state')))
      state)))

(defn exec-iteration-with-pallet-and-layer-thickness
  [pallet-variant
   {:keys [dim] :as layer}
   {:keys [boxes box-xs box-volume pallet-volume] :as input}
   & {:keys [packing-order-boxes]}]
  (let [[_px py pz] pallet-variant

        init-state
        (cond-> (inject-optimization-fields-to-state
                 {:boxes boxes
                  :layer-thickness dim
                  :packed-y 0
                  :remain-pz pz
                  :remain-py py
                  :layer-in-layer nil
                  :pallet pallet-variant}
                 input)
          packing-order-boxes
          (assoc :packing-order-boxes (vec packing-order-boxes)))]

    (loop [state init-state]
      (let [{:keys [packed-y layer-thickness
                    layer-in-layer pre-layer layer-in-layer-z]
             :as state'}
            (pack-layer state input)

            state'
            (if layer-in-layer
              (pack-layer
               (merge state' {:remain-py (- layer-thickness pre-layer)
                              :packed-y (+ pre-layer packed-y)
                              :remain-pz layer-in-layer-z
                              :layer-thickness layer-in-layer})
               input)
              state')

            packed-y' (+ packed-y layer-thickness)
            remain-py' (- py packed-y')

            state'
            (assoc state'
                   :packed-y packed-y'
                   :remain-py remain-py'
                   :remain-pz pz)

            {:keys [packing layer-thickness]}
            (find-layer state')

            state'''
            (-> state'
                (assoc :layer-thickness layer-thickness)
                (dissoc :layer-in-layer))]

        (if packing
          (recur state''')
          state''')))))

(defn exec-iterations
  [{:keys [boxes box-volume pallet-dims pallet-volume] :as input}]
  (loop [[pallet-variant & rest-pallet-variants]
         (->> (range (if (cube? pallet-dims) 1 6))
              (map (partial make-rotation pallet-dims)))

         state {:best-volume 0}]
    (if pallet-variant
      (let [{:keys [hundred-percent?] :as state'}
            (exec-iteration-on-pallet-variant
             state pallet-variant input)]
        (if hundred-percent? state' (recur rest-pallet-variants state')))
      state)))
