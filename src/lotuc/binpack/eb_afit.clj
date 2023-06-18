(ns lotuc.binpack.eb-afit
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def MAX_SIZE 32767)

(defn make-rotation
  ([[x y z] variant]
   (case variant
     0 [x y z]
     1 [z y x]
     2 [z x y]
     3 [y x z]
     4 [x z y]
     5 [y z x])))

(defn rotate-dims
  [[dim1 dim2 dim3]]
  [[dim1 dim2 dim3]
   [dim2 dim1 dim3]
   [dim3 dim1 dim2]])

(defn input-box-list-read-line [line]
  (let [[_ _lbl dim0 dim1 dim2 boxn]
        (re-find #"(.+)\.\s+(.+),\s+(.+),\s+(.+),\s+(.+)" line)
        n (parse-long boxn)
        dim0 (parse-long dim0)
        dim1 (parse-long dim1)
        dim2 (parse-long dim2)
        box {:dims [dim0 dim1 dim2]
             :vol  (* dim0 dim1 dim2)
             :n n}]
    [n box]))

(defn input-box-list [lines]
  (let [handle-pallet-line
        (fn [line]
          (let [[xx yy zz]
                (->> (s/split line #",")
                     (map s/trim)
                     (map parse-long))]
            {:pallet-volume (* xx yy zz)
             :pallet-dims [xx yy zz]}))

        handle-box-line
        (fn [[boxes box-vol] line]
          (let [[n box] (input-box-list-read-line line)]
            [(reduce (fn [boxes' _] (conj boxes' box)) boxes (range n))
             (+ box-vol (* n (:vol box)))]))

        [boxes box-vol]
        (reduce handle-box-line [[] 0] (rest lines))]
    (merge
     (handle-pallet-line (first lines))
     {:boxes boxes :box-volume box-vol})))

(defn initialize [in-txt]
  (-> in-txt s/split-lines input-box-list))

(defn list-candit-layers
  "LISTS ALL POSSIBLE LAYER HEIGHTS BY GIVING A WEIGHT VALUE TO EACH OF THEM."
  [{:keys [boxes] :as input} [px py pz]]
  (->> (for [x (range (count boxes))

             [exdim dimen2 dimen3]
             (rotate-dims (:dims (boxes x)))

             :when
             (and (<= exdim py)
                  (or (and (<= dimen2 px) (<= dimen3 pz))
                      (and (<= dimen3 px) (<= dimen2 pz))))]
         [exdim (delay (let [eval
                             (->> (for [z (range (count boxes))
                                        :when (not= x z)
                                        :let [box-z (boxes z)]]
                                    (->> (:dims box-z)
                                         (map #(abs (- exdim %)))
                                         (apply min)))
                                  (apply +))]
                         {:eval eval :dim exdim}))])
       (into {})
       (map (comp deref second))))

(defn sort-layers [layers]
  (sort-by :eval layers))

(defn find-smallest-z
  "Array of {:keys [cumz cumx]}"
  [scrap-pad]
  (if (empty? scrap-pad)
    nil
    (loop [ret 0 i 1 vs (rest scrap-pad)]
      (if-some [{:keys [cumz]} (first vs)]
        (let [smallest (if (< cumz (get-in scrap-pad [ret :cumz])) i ret)]
          (recur smallest (inc i) (rest vs)))
        ret))))

(defn analyze-box
  "ANALYZES EACH UNPACKED BOX TO FIND THE BEST FITTING ONE TO THE EMPTY SPACE
  GIVEN

  Figure 3-6: Findbox Function parameters.

  - gap: the gap paraemters
  - box: the box analyzing
  - best-fits: best fits up to current
      - bf: the box fits in the current layer
      - b-bf: the box exceeds the current layer
  "
  [{:keys [hmx hy hmy hz hmz]
    :as gap}
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
  [gap best-fits {[dim1 dim2 dim3] :dims :as box}]
  (if (and (= dim1 dim2) (= dim2 dim3))
    (analyze-box gap box best-fits)
    (->> (range 6)
         (map #(update box :dims make-rotation %))
         (reduce (partial analyze-box gap) best-fits))))

(defn find-box
  "FINDS THE MOST PROPER BOXES BY LOOKING AT ALL SIX POSSIBLE ORIENTATIONS, EMPTY
  SPACE GIVEN, ADJACENT BOXES, AND PALLET LIMITS"
  [{:keys [hmx hy hmy hz hmz] :as gap} boxes]
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
          (loop [xs (->> (range n) (map #(+ % index)))
                 b  nil]
            (if-let [i (-> xs first)]
              (let [{:keys [packst] :as b'} (assoc (boxes i) :index i)]
                (if packst (recur (rest xs) b') b'))
              b)))

        ;; first box of the next box type.
        next-box-type
        (fn [y] (+ y (:n (boxes y))))]

    (loop [i 0 best-fits init-best-fits]
      (let [{:keys [packst] :as b}
            (when (< i tbn) (try-find-unpacked-box i (:n (boxes i))))]
        (if (or (nil? b) (>= i tbn))
          best-fits
          (let [best-fits' (if packst best-fits
                               (analyze-box-with-rotation gap best-fits b))]
            (recur (next-box-type i) best-fits')))))))

(defn apply-found-box
  [{:keys [smallest-z scrap-pad] :as state}
   {[dimx _ dimz] :dims :as box}]
  (let [{:keys [cumx cumz] :as smallest-z-gap} (scrap-pad smallest-z)

        scrap-pad-before (subvec scrap-pad 0 smallest-z)
        scrap-pad-after (subvec scrap-pad (inc smallest-z))

        {pre-cumx :cumx pre-cumz :cumz :as pre} (last scrap-pad-before)
        {pos-cumz :cumz :as pos} (first scrap-pad-after)

        left-depth (if pre (- pre-cumz cumz) 0)
        right-depth (if pos (- pos-cumz cumz) 0)]
    (into
         ;; left side
     (if (= dimz left-depth)
       (into [] (drop-last scrap-pad-before))
       scrap-pad-before)

     (if (= (+ (or pre-cumx 0) dimx) cumx) ; fill the x direction
       (cond
         (= dimz right-depth)
         (into [] scrap-pad-after)

         (> dimz right-depth)
         (into [{:cumx cumx :cumz (+ cumz dimz)}] (rest scrap-pad-after))

         :else
         (into [{:cumx cumx :cumz (+ cumz dimz)}] scrap-pad-after))
       (into [{:cumx (+ (or pre-cumx 0) dimx) :cumz (+ cumz dimz)} smallest-z-gap]
             scrap-pad-after)))))

(defn apply-volume-check
  [{:keys [boxes] :as state} c-box
   {:keys [box-volume pallet-volume]}]
  (let [{:keys [packed-volume] :as state'}
        (-> state
            (assoc [:boxes (:index c-box) :packst] true)
            (assoc-in [:boxes (:index c-box) :pack] c-box)
            (update :packed-volume #(+ (or % 0) (:vol c-box)))
            (update :packed-number #(inc (or % 0))))]
    (cond-> state'
      (or (= packed-volume box-volume) (= packed-volume pallet-volume))
      (assoc :packing false :hundred-percent 1))))

(defn apply-ignore-gap
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

          {pre-cumz :cumz :as pre} (last scrap-pad-before)
          {pos-cumx :cumx pos-cumz :cumz :as pos} (first scrap-pad-after)]
      (cond
        (= pre-cumz pos-cumz)
        (into (into [] (drop-last scrap-pad-before))
              (rest scrap-pad-after))

        (< pre-cumz pos-cumz)
        (into (conj (into [] (drop-last scrap-pad-before))
                    {:cumx pos-cumx :cumz pre-cumz})
              scrap-pad-after)

        ;; (> pre-cumz pos-cumz)
        :else
        (into (conj (into [] (drop-last scrap-pad-before))
                    {:cumx pos-cumx :cumz pre-cumz})
              (rest scrap-pad-after))))))

(defn check-found
  "returns [state', {layer-done? ignore-gap? c-box}]."
  [{:keys [box b-box]
    :as find-box-res}
   {:keys [layer-in-layer layer-thickness smallest-z scrap-pad]
    :as state}]
  (cond
      ;; found a box and its thickness fits in current layer.
    box
    [state {:c-box box}]

      ;; found a box which triggers the layer-in-layer mode or update the
      ;; layer-in-layer parameters.
    (and b-box (or layer-in-layer (= 1 (count scrap-pad))))
    (let [b-box-y (get-in b-box [:dims 1])
          pre-layer (when-not layer-in-layer layer-thickness)
          layer-in-layer-z (when-not layer-in-layer
                             (get-in scrap-pad [smallest-z :cumz]))
          layer-in-layer (- (+ (or layer-in-layer 0) b-box-y) layer-thickness)]
      [(assoc state
              :layer-in-layer layer-in-layer
              :layer-thickness b-box-y
              :pre-layer pre-layer
              :layer-in-layer-z layer-in-layer-z)
       {:c-box b-box}])

      ;; got one gap & found no box; notice here (true? (= 1 (count scrap-pad)))
      ;; indicates (nil? b-box)
    (= 1 (count scrap-pad))
    [state {:layer-done? true}]

      ;; ignore the current gap (evened)
    :else
    [state {:ignore-gap? true}]))

(defn pack-layer
  "PACKS THE BOXES FOUND AND ARRANGES ALL VARIABLES AND RECORDS PROPERLY"
  [{{:keys [px py pz]} :pallet
    :as state}
   input]
  (loop [{:keys [scrap-pad boxes layer-thickness remain-pz remain-py] :as state}
         (assoc state :scrap-pad [{:cumx px :cumz 0}])]
    (let [smallest-z (find-smallest-z scrap-pad)
          state (assoc state :smallest-z smallest-z)
          gap (cond
                (= 1 (count scrap-pad))
                ;; SITUATION-1: NO BOXES ON THE RIGHT AND LEFT SIDES
                (let [{:keys [cumx]} (scrap-pad smallest-z)
                      hz  (- remain-pz (:cumz smallest-z))]
                  {:hmx cumx :hy layer-thickness :hmy remain-py :hz hz :hmz hz})

                (zero? smallest-z)
                ;; SITUATION-2: NO BOXES ON THE LEFT SIDE
                (let [{:keys [cumx cumz]} (scrap-pad smallest-z)]
                  {:hmx cumx
                   :hy layer-thickness
                   :hmy remain-py
                   :hz (- (get-in scrap-pad [(inc smallest-z) :cumz]) cumz)
                   :humz (- remain-pz cumz)})

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
                   :humz (- remain-pz cumz)})

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
                   :hmz (- remain-pz cumz)}))

          [state' {:keys [layer-done? ignore-gap? c-box]}]
          (-> (find-box gap boxes) (check-found state))]
      (if layer-done?
        state'
        (recur (cond-> state'
                 ignore-gap? (assoc :scrap-pad (apply-ignore-gap state'))
                 c-box (assoc :scrap-pad (apply-found-box state' c-box))
                 c-box (apply-volume-check c-box input)))))))

(defn find-layer
  [{:keys [boxes remain-py {:keys [px py pz]} :pallet] :as state}]
  (let [[_ layer-thickness]
        (->> (for [x (range (count boxes))
                   [exdim dim2 dim3] (rotate-dims (:dims (boxes x)))
                   :when (and (not (get-in boxes [x :packst]))
                              (< exdim remain-py)
                              (or (and (< dim2 px) (< dim3 pz))
                                  (and (< dim3 px) (< dim2 pz))))
                   :let [layer-eval
                         (->> (for [z (range (count boxes))
                                    :when (and (not= x z)
                                               (not (get-in boxes [z :packst])))
                                    :let [{:keys [dx dy dz]} (:dims (boxes z))]]
                                (min (abs (- exdim dx))
                                     (abs (- exdim dy))
                                     (abs (- exdim dz))))
                              (apply +))]]
               [layer-eval exdim])
             (reduce (fn [[layer-eval exdim] [new-layer-eval new-exdim]]
                       (if (< new-layer-eval layer-eval)
                         [new-layer-eval new-exdim]
                         [layer-eval exdim]))
                     [1000000 0]))]
    (if (or (= layer-thickness 0) (> layer-thickness remain-py))
      (assoc state :packing false)
      (assoc state :layer-thickness layer-thickness))))

(defn exec-iteration
  [{:keys [boxes box-volume pallet-dims pallet-volume] :as input}
   variant]
  (let [{:keys [_px py pz] :as pallet'}
        (make-rotation pallet-dims variant)

        layers
        (-> (list-candit-layers input pallet') sort-layers)]
    (doseq [[_itelayer {:keys [layer-dim]}]
            (map-indexed (fn [i v] [i v]) layers)]
      (loop [state {:boxes boxes
                    :layer-thickness layer-dim
                    :packed-by 0
                    :remain-pz pz
                    :remain-py py
                    :layer-in-layer nil
                    :pallet pallet'}]
        (let [{:keys [packed-by remain-py layer-thickness
                      layer-in-layer pre-layer layer-in-layer-z]
               :as state'}
              (pack-layer state input)

              state''
              (if layer-in-layer
                (merge
                 (pack-layer
                  (merge state'
                         {:pre-packed-by packed-by
                          :pre-remain-py remain-py
                          :remain-py (- layer-thickness pre-layer)
                          :packed-by (+ (- packed-by layer-thickness) pre-layer)
                          :remain-pz layer-in-layer-z
                          :layer-thickness layer-in-layer})
                  input)
                 {:packed-by packed-by
                  :remain-py remain-py
                  :remain-pz pz})
                state')

              {:keys [packing] :as state'''}
              (find-layer state'')]
          (when packing
            (recur (find-layer state''))))))))

(comment
  (def input (initialize (slurp (io/resource "dpp00.txt"))))
  (sort-layers (list-candit-layers input (:pallet-dims input)))
  (find-box {:hmx 69 :hy 20 :hmy 100 :hz 20 :hmz 40} (:boxes input))
  (exec-iteration input 0)
  (input-box-list-read-line "1. 70, 104, 24, 4"))
