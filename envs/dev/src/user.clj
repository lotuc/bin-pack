(ns user
  (:require [lambdaisland.classpath.watch-deps :as watch-deps]))

(watch-deps/start! {:aliases [:dev :test]})
