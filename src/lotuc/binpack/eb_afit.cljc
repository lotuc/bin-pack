(ns lotuc.binpack.eb-afit
  "eb stands for the paper's author Erhan Baltacioglu, and afit stands for Air
  Force Institude of Technology."
  (:require
   [clojure.set :as set]))

(declare exec-iterations)

(defn find-best-pack
  "Find the best packing strategy for given input using the algorithm described in
  paper https://scholar.afit.edu/etd/4563/.

  The reference implemention described in the paper can be found here at
  https://github.com/wknechtel/3d-bin-pack.

  The implementation is a modified version.

  Parameter
  - input
      - pallet-dims: a vector of the pallet's 3 dimensions [px py pz]
      - pallet-volume: the pallet volume (* px py pz)
      - boxes: a vector of boxes, each of its item is
          - dims: a vector of the box's 3 dimensions [dim1 dim2 dim3]
          - vol: volume of the box (* dim1 dim2 dim3)
          - n: represents the count of box with such a dims.
      - box-volume: the sum of all boxes' vol.

  Returns the best packing results
  - input: same as the parameter
  - packed-volume: sum volume of the packed boxes
  - packed-number: number of boxes packed
  - pallet-variant: the pack strategy's pallet variant
  - layer: the pack strategy's initial layer thickness (dim) and its weight
     (weight).
  - pack: the packed result, it's the input.boxes with pack result fields (for
    the boxes that are packed)

  ```
  (find-best-pack {:pallet-volume 1000,
                   :pallet-dims [10 10 10],
                   :boxes [{:dims [10 10 10], :vol 1000, :n 1}],
                   :box-volume 1000})
  ;; ->
  {:input
   {:pallet-volume 1000,
    :pallet-dims [10 10 10],
    :boxes [{:dims [10 10 10], :vol 1000, :n 1}],
    :box-volume 1000},
   :percentage-used 100.0,
   :packed-volume 1000,
   :packed-number 1,
   :pallet-variant [10 10 10],
   :layer {:weight 0, :dim 10},
   :pack
   [{:dims [10 10 10],
     :vol 1000,
     :n 1,
     :pack-dims [10 10 10],
   :pack-coord [0 0 0]}]}
  ```
  "
  [input]
  (-> (exec-iterations input)
      (set/rename-keys
       {:best-volume :packed-volume
        :best-packed-number :packed-number
        :best-pallet-variant :pallet-variant
        :best-layer :layer
        :best-pack :pack})))

(comment
  #_{:clj-kondo/ignore [:unresolved-namespace]}
  (-> (lotuc.binpack.eb-afit-io/read-input-from-resource "test-dpp00.txt")
      find-best-pack))

(def MAX_SIZE 32767)

(defn make-rotation
  ([[x y z] variant]
   (case variant
     0 [x y z] 1 [z y x] 2 [z x y] 3 [y x z] 4 [x z y] 5 [y z x])))

(defn rotate-dim1 [[dim1 dim2 dim3]]
  [[dim1 dim2 dim3] [dim2 dim1 dim3] [dim3 dim1 dim2]])

(defn cube? [[dim1 dim2 dim3]]
  (and (= dim1 dim2) (= dim2 dim3)))

(defn list-candit-layers
  "LISTS ALL POSSIBLE LAYER HEIGHTS BY GIVING A WEIGHT VALUE TO EACH OF THEM."
  [{:keys [boxes] :as input} [px py pz]]
  (->> (for [x (range (count boxes))

             [exdim dimen2 dimen3]
             (rotate-dim1 (:dims (boxes x)))

             :when
             (and (<= exdim py)
                  (or (and (<= dimen2 px) (<= dimen3 pz))
                      (and (<= dimen3 px) (<= dimen2 pz))))]
         [exdim (delay (let [weight
                             (->> (for [z (range (count boxes))
                                        :when (not= x z)
                                        :let [box-z (boxes z)]]
                                    (->> (:dims box-z)
                                         (map #(abs (- exdim %)))
                                         (apply min)))
                                  (apply +))]
                         {:weight weight :dim exdim}))])
       (into {})
       (map (comp deref second))))

(defn sort-layers [layers]
  (sort-by :weight layers))

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
  [gap best-fits {:keys [dims] :as box}]
  (->> (range (if (cube? dims) 1 6))
       (map #(update box :dims make-rotation %))
       (reduce (partial analyze-box gap) best-fits)))

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
              (let [{:keys [pack-dims] :as b'} (assoc (boxes i) :index i)]
                (if pack-dims (recur (rest xs) b') b'))
              b)))

        ;; first box of the next box type.
        next-box-type
        (fn [y] (+ y (:n (boxes y))))]

    (loop [i 0 best-fits init-best-fits]
      (let [{:keys [pack-dims] :as b}
            (when (< i tbn) (try-find-unpacked-box i (:n (boxes i))))]
        (if (or (nil? b) (>= i tbn))
          best-fits
          (let [best-fits' (if pack-dims best-fits
                               (analyze-box-with-rotation gap best-fits b))]
            (recur (next-box-type i) best-fits')))))))

(defn apply-volume-check
  [{:keys [packed-y boxes smallest-z scrap-pad] :as state}
   {:keys [index dims] :as found-box}
   {:keys [box-volume pallet-volume] :as input}]
  (let [box-vol
        (get-in boxes [index :vol])

        cox (if (zero? smallest-z)
              0 (get-in scrap-pad [(dec smallest-z) :cumx]))
        coz (get-in scrap-pad [smallest-z :cumz])
        coy packed-y

        {:keys [packed-volume] :as state'}
        (-> state
            (assoc-in [:boxes index :pack-dims] dims)
            (assoc-in [:boxes index :pack-coord] [cox coy coz])
            (update :packed-volume #(+ (or % 0) box-vol))
            (update :packed-number #(inc (or % 0))))]
    (cond-> state'
      (or (= packed-volume box-volume) (= packed-volume pallet-volume))
      (assoc :packing false :hundred-percent? true))))

(defn apply-found-box
  [{:keys [smallest-z scrap-pad] :as state}
   {:keys [index] [dimx _ dimz] :dims :as found-box}]
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

          {:keys [cumx]} (scrap-pad smallest-z)
          {pre-cumz :cumz :as pre} (last scrap-pad-before)
          {pos-cumz :cumz :as pos} (first scrap-pad-after)]
      (if (= pre-cumz pos-cumz)
        (into (into [] (drop-last scrap-pad-before))
              scrap-pad-after)

        (into (conj (into [] (drop-last scrap-pad-before))
                    {:cumx cumx :cumz pre-cumz})
              scrap-pad-after)))))

(defn check-found
  "returns [state', {layer-done? ignore-gap? c-box}]."
  [{:keys [box b-box]
    :as find-box-res}
   {:keys [layer-thickness smallest-z scrap-pad
           layer-in-layer pre-layer layer-in-layer-z]
    :as state}]
  (cond
    ;; found a box and its thickness fits in current layer.
    box
    [state {:c-box box}]

    ;; found a box which triggers the layer-in-layer mode or update the
    ;; layer-in-layer parameters.
    (and b-box (or layer-in-layer (= 1 (count scrap-pad))))
    (let [b-box-y (get-in b-box [:dims 1])
          pre-layer (if layer-in-layer pre-layer layer-thickness)
          layer-in-layer-z (if layer-in-layer layer-in-layer-z
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
  [{[px _py _pz] :pallet :as state} input]
  (loop [{:keys [scrap-pad boxes layer-thickness remain-pz remain-py] :as state}
         (assoc state :scrap-pad [{:cumx px :cumz 0}])]
    (let [smallest-z (find-smallest-z scrap-pad)
          state (assoc state :smallest-z smallest-z)
          gap (cond
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
                   :hmz (- remain-pz cumz)}))

          [state' {:keys [layer-done? ignore-gap? c-box]}]
          (-> (find-box gap boxes) (check-found state))]
      (if layer-done?
        state'
        (recur (cond-> state'
                 ignore-gap? (assoc :scrap-pad (apply-ignore-gap state'))
                 c-box (apply-volume-check c-box input)
                 c-box (assoc :scrap-pad (apply-found-box state' c-box))))))))

(defn find-layer
  [{:keys [boxes remain-py]
    [px _py pz] :pallet
    :as state}]
  (let [[_ layer-thickness]
        (->> (for [x (range (count boxes))
                   [exdim dim2 dim3] (rotate-dim1 (:dims (boxes x)))
                   :when (and (not (get-in boxes [x :pack-dims]))
                              (<= exdim remain-py)
                              (or (and (<= dim2 px) (< dim3 pz))
                                  (and (<= dim3 px) (< dim2 pz))))
                   :let [layer-weight
                         (->> (for [z (range (count boxes))
                                    :when (and (not= x z)
                                               (not (get-in boxes [z :pack-dims])))
                                    :let [[dx dy dz] (:dims (boxes z))]]
                                (min (abs (- exdim dx))
                                     (abs (- exdim dy))
                                     (abs (- exdim dz))))
                              (apply +))]]
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
(declare exec-iteration-on-pallet-variant-with-layer)

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
            (exec-iteration-on-pallet-variant-with-layer
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
          state'
          (recur rest-layers state')))
      state)))

(defn exec-iteration-on-pallet-variant-with-layer
  [pallet-variant
   {:keys [dim] :as layer}
   {:keys [boxes box-volume pallet-volume] :as input}]
  (let [[_px py pz] pallet-variant]
    (loop [state {:boxes boxes
                  :layer-thickness dim
                  :packed-y 0
                  :remain-pz pz
                  :remain-py py
                  :layer-in-layer nil
                  :pallet pallet-variant}]
      (let [{:keys [packed-y layer-thickness
                    layer-in-layer pre-layer layer-in-layer-z]
             :as state'}
            (pack-layer state input)

            packed-y (+ packed-y layer-thickness)
            remain-py (- py packed-y)

            state''
            (if layer-in-layer
              (merge
               (pack-layer
                (merge state'
                       {:remain-py (- layer-thickness pre-layer)
                        :packed-y (+ (- packed-y layer-thickness) pre-layer)
                        :remain-pz layer-in-layer-z
                        :layer-thickness layer-in-layer})
                input)
               {:packed-y packed-y
                :remain-py remain-py
                :remain-pz pz})
              (assoc state'
                     :packed-y packed-y
                     :remain-py remain-py))

            {:keys [packing layer-thickness]}
            (find-layer state'')

            state'''
            (assoc state'' :layer-thickness layer-thickness)]
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
