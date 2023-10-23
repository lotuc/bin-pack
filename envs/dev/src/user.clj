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

(defn find-best-pack-flame
  [& {:keys [n time-bound-in-millis use-pmap]}]
  (let [input (eb-afit-io/read-input-from-resource n)]
    (start-flame)
    (println "========================================")
    (println (count (:boxes input)))
    (let [r (time (eb-afit/find-best-pack
                   input {:time-bound-in-millis time-bound-in-millis
                          :use-pmap use-pmap}))
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
    (let [{:keys [res-ch in-ch out-ch]} (eb-afit-async/find-best-pack input)
          !progress (atom 0)]
      (a/go-loop []
        (let [[_action data :as v] (a/<! out-ch)]
          ;; action is always :pause now.
          ;; decide if continue on data.
          (let [[p0 p1] (reset-vals! !progress (eb-afit-async/calc-progress (:statistics data)))]
            (when-not (= p0 p1) (println :progress p1)))
          (when v
            (a/>! in-ch :continue)
            (recur))))
      (let [r (time (a/<!! res-ch))
            r (-> r
                  (dissoc :input :pack)
                  (assoc :total-number (count (:input r))))]
        (println r)))
    (println "========================================")))

(comment
  (find-best-pack-flame {:n "3d-bin-pack-test/mpp05.txt"
                         :time-bound-in-millis 900})
  (find-best-pack-flame {:n "3d-bin-pack-test/mpp03.txt"})
  (find-best-pack-flame {:n "3d-bin-pack-test/mpp03.txt" :use-pmap true})

  (async-find-best-pack-flame {:n "3d-bin-pack-test/mpp03.txt"}))
