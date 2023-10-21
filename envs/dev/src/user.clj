(ns user
  (:require
   [clojure.core.async :as a]
   [flames.core :as flames]
   [lambdaisland.classpath.watch-deps :as watch-deps]
   [lotuc.binpack.eb-afit :as eb-afit]
   [lotuc.binpack.eb-afit-async :as eb-afit-async]
   [lotuc.binpack.eb-afit-io :as eb-afit-io]))

(defonce watch-deps-started (atom nil))
(when (and (not @watch-deps-started) (swap! watch-deps-started not))
  (watch-deps/start! {:aliases [:dev :test]}))

(defonce flames (atom nil))

(defn stop-flame []
  (->> #(do (when % (.close %)) nil)
       (swap! flames)))

(defn start-flame []
  (->> #(do (when % (.close %)) (flames/start! {:port 54321, :host "localhost"}))
       (swap! flames)))

(defn watch-statistics [!statistics]
  (let [!progress (atom 0)
        calc-progress (fn [{:keys [pallet-variants running-pallet-variants]}]
                        (cond
                          (nil? pallet-variants) 0
                          (empty? pallet-variants) 100
                          :else
                          (->> (vals running-pallet-variants)
                               (map (fn [{:keys [total-layer-thicknesses finished-layer-thicknesses finished?]}]
                                      (cond
                                        finished? 1
                                        (nil? total-layer-thicknesses) 0
                                        (zero? total-layer-thicknesses) 1
                                        (and total-layer-thicknesses finished-layer-thicknesses) (/ (* finished-layer-thicknesses 1.0) total-layer-thicknesses)
                                        :else 0)))
                               (reduce + 0.0)
                               (#(Math/ceil (* 100 (/ % (count pallet-variants))))))))]
    (add-watch
     !progress :watch
     (fn [_ _ p0 p1] (when-not (= p0 p1) (println "Progress:" p1))))
    (add-watch
     !statistics :watch
     (fn [_ _ _ {:keys [pallet-variants running-pallet-variants] :as v}]
       (reset! !progress (calc-progress v))))))

(defn find-best-pack-flame
  [& {:keys [n time-bound-in-millis use-pmap on-statistics]}]
  (let [input (eb-afit-io/read-input-from-resource n)]
    (start-flame)
    (println "========================================")
    (println (count (:boxes input)))
    (let [r (time (eb-afit/find-best-pack
                   input {:time-bound-in-millis time-bound-in-millis
                          :use-pmap use-pmap
                          :on-statistics on-statistics}))
          r (-> r
                (dissoc :input :pack)
                (assoc :total-number (count (:input r))))]
      (println r))
    (println "========================================")))

(defn async-find-best-pack-flame
  [& {:keys [n]}]
  (let [input (eb-afit-io/read-input-from-resource n)]
    (start-flame)
    (println "========================================")
    (println (count (:boxes input)))
    (let [r (time (a/<!! (:res-ch (eb-afit-async/find-best-pack input))))
          r (-> r
                (dissoc :input :pack)
                (assoc :total-number (count (:input r))))]
      (println r))
    (println "========================================")))

(comment
  (find-best-pack-flame {:n "3d-bin-pack-test/mpp05.txt"
                         :time-bound-in-millis 900})
  (find-best-pack-flame {:n "3d-bin-pack-test/mpp03.txt"})
  (find-best-pack-flame {:n "3d-bin-pack-test/mpp03.txt"
                         :use-pmap true})

  (async-find-best-pack-flame {:n "3d-bin-pack-test/mpp03.txt"})

  (find-best-pack-flame
   {:n "3d-bin-pack-test/mpp03.txt"
    :use-pmap true
    :on-statistics watch-statistics}))
