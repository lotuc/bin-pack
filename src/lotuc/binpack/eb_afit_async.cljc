(ns lotuc.binpack.eb-afit-async
  "eb stands for the paper's author Erhan Baltacioglu, and afit stands for Air
  Force Institude of Technology."
  #?(:clj (:refer-clojure :exclude [get-in assoc-in assoc]))
  #?(:cljs (:require-macros [lotuc.binpack.eb-afit-macro :refer [go go! swap-stats!]]))
  (:require
   #?(:clj [clj-fast.clojure.core :refer [assoc assoc-in]])
   #?(:clj [lotuc.binpack.eb-afit-macro :refer [go go! swap-stats!]])
   [clojure.core.async :as a]
   [lotuc.binpack.eb-afit :as eb-afit]))

#?(:clj (set! *unchecked-math* true))

(def ^:private ^:dynamic *ctx* {})
(def ^:private ^:dynamic *stats* nil)

(defn- continue? []
  (go
    (let [{:keys [out-ch in-ch]} *ctx*]
      (when-not (a/>! out-ch [:pause {:statistics (when *stats* @*stats*)}])
        (a/close! in-ch))
      (let [v (a/<! in-ch)
            continue? (= v :continue)]
        (when (nil? v)
          (a/close! out-ch))
        (when-not continue?
          (a/close! in-ch)
          (a/close! out-ch))
        continue?))))

(declare exec-iterations)
(declare exec-iteration-on-pallet-variant)
(declare exec-iteration-on-pallet-variant-with-layer-thickness)

(defn calc-progress
  "This is a sample progress calculation function."
  [{:keys [pallet-variants running-pallet-variants] :as statistics}]
  (cond
    (nil? pallet-variants) 0
    (empty? pallet-variants) 100
    :else
    (let [vs (vals running-pallet-variants)]
      (if (some :hundred-percent? vs)
        100.0
        (->> vs
             (map (fn [{:keys [total-layer-thicknesses
                               finished-layer-thicknesses
                               finished?]}]
                    (cond
                      finished? 1
                      (nil? total-layer-thicknesses) 0

                      (and total-layer-thicknesses finished-layer-thicknesses)
                      (/ (* (inc finished-layer-thicknesses) 1.0) (inc total-layer-thicknesses))
                      :else 0)))
             (reduce + 0.0)
             (#(* 100 (/ % (count pallet-variants)))))))))

(defn find-best-pack
  "Async version of `lotuc.binpack.eb-afit/find-best-pack`.

  ```
  (require '[clojure.core.async :as a])
  (let [{:keys [res-ch in-ch out-ch]}
        (find-best-pack {:pallet-dims [10 5 5],
                         :boxes [{:dims [5 5 5], :n 3}]})
        !progress (atom 0)]
    (a/go-loop []
      (let [[action data :as v] (a/<! out-ch)]
        ;; action is always :pause now.
        ;; decide if continue on the statistics data or your time bounds.
        (let [[p0 p1] (reset-vals! !progress (calc-progress (:statistics data)))]
          (when-not (= p0 p1) (println :progress p1)))
        (when v
          (a/>! in-ch :continue)
          (recur))))
    #?(:clj (a/<!! res-ch)
       :cljs (a/go (println :res (a/<! res-ch)))))
  ;; ->
  {:pallet-volume 250,
   :box-volume 375,
   :packed-volume 250,
   :pallet-variant [10 5 5],
   :packed-number 2,
   :percentage-used 100.0,
   :pack
   [{:pack-dims [5 5 5], :pack-coord [0 0 0], :dims [5 5 5], :n 3, :vol 125}
    {:pack-dims [5 5 5], :pack-coord [5 0 0], :dims [5 5 5], :n 3, :vol 125}],
   :first-layer-thickness 5,
   :unpacked [{:dims [5 5 5], :n 3, :vol 125, :unpacked-n 1}]}"
  [input]
  (let [out-ch (a/chan)
        in-ch (a/chan)
        {:keys [pallet-volume box-volume] :as iteration-input} (eb-afit/build-iteration-input input)]
    (binding [*ctx* {:out-ch out-ch :in-ch in-ch}
              *stats* (atom nil)]
      (let [res-ch
            (go! (let [data (a/<! (exec-iterations iteration-input))]
                   (when-some [{:keys [best-pack best-layer-thickness best-volume
                                       best-packed-number best-pallet-variant percentage-used]}
                               data]
                     {:packed-volume best-volume
                      :packed-number best-packed-number
                      :pallet-variant best-pallet-variant
                      :percentage-used percentage-used
                      :first-layer-thickness best-layer-thickness
                      :pack (filter :pack-dims best-pack)
                      :unpacked (eb-afit/build-unpacked best-pack)
                      :pallet-volume pallet-volume
                      :box-volume box-volume})))]
        {:out-ch out-ch :in-ch in-ch :res-ch res-ch}))))

(defn- on-total-pallet-variants [variants]
  (swap-stats! assoc :pallet-variants variants))

(defn- on-pallet-variant-start [variant-num]
  (swap-stats! assoc-in [:running-pallet-variants variant-num] {}))

(defn- on-pallet-variant-finished [variant-num hundred-percent?]
  (swap-stats! assoc-in [:running-pallet-variants variant-num :finished?] true)
  (when hundred-percent?
    (swap-stats! assoc-in [:running-pallet-variants variant-num :hundred-percent?] true)))

(defn- on-pallet-variant-total-layer-thickness [variant-num total-layer-thickness]
  (swap-stats! assoc-in [:running-pallet-variants variant-num :total-layer-thicknesses] total-layer-thickness))

(defn- on-pallet-variant-layer-thickness-start [variant-num layer-thickness]
  (swap-stats! assoc-in [:running-pallet-variants variant-num :running-layer-thicknesses] layer-thickness))

(defn- on-pallet-variant-layer-thickness-finished [variant-num _layer-thickness]
  (swap-stats! update-in [:running-pallet-variants variant-num :finished-layer-thicknesses] (fnil inc 0)))

(defn exec-iterations
  [{:keys [boxes box-volume pallet-dims pallet-volume]
    :as iteration-input}]
  (go
    (let [n #?(:clj (+ 2 (.. Runtime getRuntime availableProcessors)) :cljs 1)
          ps-seq (->> (range (if (eb-afit/cube? pallet-dims) 1 6))
                      (map (partial eb-afit/make-rotation pallet-dims))
                      ((fn [v] (on-total-pallet-variants v) v))
                      (map-indexed
                       (fn [i v]
                         (fn [] (go
                                 (on-pallet-variant-start i)
                                 (let [v (a/<! (exec-iteration-on-pallet-variant iteration-input v i))]
                                   (on-pallet-variant-finished i (:hundred-percent? v))
                                   v))))))]
      (a/<!
       (go
         (loop [res nil
                ps (set (map (fn [p] (p)) (take n ps-seq)))
                ps-seq (nthrest ps-seq n)]
           (if (empty? ps)
             res
             (let [[{:keys [hundred-percent? best-volume] :as v} ch] (a/alts! (vec ps))
                   res (cond
                         hundred-percent?                   v
                         (nil? res)                         v
                         (or (nil? v) (zero? best-volume))  res
                         (> best-volume (:best-volume res)) v
                         :else                              res)
                   [p0 & prest] ps-seq]
               (when (continue?)
                 (if hundred-percent?
                   res
                   (recur res (cond-> (disj ps ch) p0 (conj (p0))) prest )))))))))))

(defn exec-iteration-on-pallet-variant
  [{:keys [pallet-volume] :as iteration-input} pallet-variant pallet-variant-num]
  (go! (let [layers (a/<! (go! (eb-afit/list-candit-layer-thickness-with-weight iteration-input pallet-variant)))]
         (on-pallet-variant-total-layer-thickness pallet-variant-num (count layers))
         (when-some [sorted-layer-thickness (->> layers (sort-by :weight) (map :dim) seq)]
           (a/<!
            (go! (loop [[layer-thickness & rest-layer-thicknesses] sorted-layer-thickness
                        {:keys [best-volume] :as state} {:best-volume 0}]
                   (if layer-thickness
                     (let [_ (on-pallet-variant-layer-thickness-start pallet-variant-num layer-thickness)
                           {:keys [packed-volume packed-number boxes hundred-percent?]}
                           (a/<! (exec-iteration-on-pallet-variant-with-layer-thickness
                                  iteration-input pallet-variant layer-thickness))
                           _ (on-pallet-variant-layer-thickness-finished pallet-variant-num layer-thickness)
                           state' (if (> packed-volume best-volume)
                                    {:best-volume packed-volume
                                     :best-pallet-variant pallet-variant
                                     :best-layer-thickness layer-thickness
                                     :best-packed-number packed-number
                                     :percentage-used (/ (* 100.0 packed-volume) pallet-volume)
                                     :best-pack boxes
                                     :input iteration-input}
                                    state)]
                       (when (continue?)
                         (if hundred-percent?
                           (assoc state' :hundred-percent? true)
                           (recur rest-layer-thicknesses state'))))
                     state))))))))

(defn exec-iteration-on-pallet-variant-with-layer-thickness
  [{:keys [boxes box-xs box-volume pallet-volume] :as iteration-input}
   pallet-variant
   layer-thickness
   & {:keys [on-pack]}]
  (let [[_px py pz] pallet-variant]
    (go! (loop [state (-> iteration-input
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
                 (a/<! (go! (eb-afit/pack-layer iteration-input state)))

                 packed-state-after-layer-in-layer
                 (a/<! (go! (-> packed-state
                                (as-> $ (if layer-in-layer
                                          ;; do layer-in-layer packing
                                          (-> (a/<! (go! (eb-afit/pack-layer
                                                          iteration-input
                                                          (merge $ {:remain-py (- layer-thickness pre-layer)
                                                                    :packed-y (+ pre-layer packed-y)
                                                                    :remain-pz layer-in-layer-z
                                                                    :layer-thickness layer-in-layer}))))
                                              (dissoc :layer-in-layer))

                                          $))
                                (merge (let [packed-y' (+ packed-y layer-thickness)
                                             remain-py' (- py packed-y')]
                                         {:packed-y packed-y'
                                          :remain-py remain-py'
                                          :remain-pz pz})))))

                 next-layer-thickness
                 (a/<! (go! (eb-afit/find-layer-thickness packed-state-after-layer-in-layer)))]

             (when (continue?)
               (if next-layer-thickness
                 (recur (-> packed-state-after-layer-in-layer
                            (assoc :layer-thickness next-layer-thickness)))
                 packed-state-after-layer-in-layer)))))))
