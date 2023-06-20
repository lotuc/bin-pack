(ns user
  (:require [flames.core :as flames]
            [lambdaisland.classpath.watch-deps :as watch-deps]
            [lotuc.binpack.eb-afit]
            [lotuc.binpack.eb-afit-io]))

(watch-deps/start! {:aliases [:dev :test]})

(comment
  (def flames (atom nil))
  (swap! flames (fn [v] (when v (.close v)) nil))

  (let [input (-> "3d-bin-pack-test/mpp02.txt"
                  lotuc.binpack.eb-afit-io/read-input-from-resource)]
    (swap! flames
           (fn [v]
             (when v (.close v))
             (flames/start! {:port 54321, :host "localhost"})))
    (println "start")
    (time (lotuc.binpack.eb-afit/find-best-pack input))
    (println "finished")))
