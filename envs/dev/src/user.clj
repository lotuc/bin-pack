(ns user
  (:require [flames.core :as flames]
            [lambdaisland.classpath.watch-deps :as watch-deps]
            [lotuc.binpack.eb-afit :as eb-afit]
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

(defn find-best-pack-flame [& {:keys [n]
                               :or {n "3d-bin-pack-test/mpp02.txt"}}]
  (let [input (eb-afit-io/read-input-from-resource n)]
    (start-flame)
    (println "========================================")
    (println (count (:boxes input)))
    (let [r (time (eb-afit/find-best-pack input))
          r (-> r
                (dissoc :input :pack)
                (assoc :total-number (count (:input r))))]
      (println r))
    (println "========================================")))

(comment
  (find-best-pack-flame))
