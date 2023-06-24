(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'org.lotuc/bin-pack)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))

(def uber-file "target/lotuc-bin-pack.jar")

(defn clean [_]
  (b/delete {:path "target"}))

(defn prep [_]
  (println "writing pom")
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir}))

(defn uber [_]
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis}))

(defn all [_]
  (clean nil) (prep nil) (uber nil))
