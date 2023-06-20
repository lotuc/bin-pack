(ns lotuc.binpack.eb-afit-test
  (:use clojure.test)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [lotuc.binpack.eb-afit :as eb-afit]
   [lotuc.binpack.eb-afit-io :as eb-afit-io]
   [lotuc.binpack.eb-afit-io-test :as eb-afit-io-test]))

(deftest apply-found-box-test
  (testing "smallest-z gap is the only gap"
    (is (= [{:cumx 20 :cumz 20}]
           (eb-afit/apply-found-box
            {:scrap-pad [{:cumx 20 :cumz 10}]
             :smallest-z 0}
            {:dims [20 10 10]}))))

  (testing "smallest-z gap has gap on right side."
    (is (= [{:cumx 20, :cumz 10}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [5 11 5]}))
        "x direction is filled -> merge to right")
    (is (= [{:cumx 5, :cumz 8} {:cumx 20, :cumz 10}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [5 11 3]}))
        "x direction is filled")
    (is (= [{:cumx 4, :cumz 8} {:cumx 5, :cumz 5} {:cumx 20, :cumz 10}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [4 11 3]}))
        "x direction is not filled"))

  ;; todo
  (testing "smallest-z gap has gap on left side.")

  ;; todo
  (testing "smallest-z gap has gap on both side."))

(deftest apply-ignore-gap-test
  (testing "the gap to ignore is in middle"
    (is (= [{:cumx 104, :cumz 84}]
           (eb-afit/apply-ignore-gap
            {:scrap-pad [{:cumx 30, :cumz 84}
                         {:cumx 36, :cumz 77}
                         {:cumx 104, :cumz 84}]
             :smallest-z 1}))
        "pre & pos's cumz is equal")
    (is (= [{:cumx 36, :cumz 90}
            {:cumx 104, :cumz 84}]
           (eb-afit/apply-ignore-gap
            {:scrap-pad [{:cumx 30, :cumz 90}
                         {:cumx 36, :cumz 77}
                         {:cumx 104, :cumz 84}]
             :smallest-z 1}))
        "pre cumz > pos cumz")
    (is (= [{:cumx 36, :cumz 82}
            {:cumx 104, :cumz 84}]
           (eb-afit/apply-ignore-gap
            {:scrap-pad [{:cumx 30, :cumz 82}
                         {:cumx 36, :cumz 77}
                         {:cumx 104, :cumz 84}]
             :smallest-z 1}))
        "pre cumz < pos cumz")))

(defn- test-find-test-pack-on-resource [resource-name-or-file]
  (println "start find-best-pack " resource-name-or-file)
  (let [in-txt (if (.exists (io/file resource-name-or-file))
                 (slurp resource-name-or-file)
                 (slurp (io/resource resource-name-or-file)))
        r (atom nil)
        costs (with-out-str
                (time
                 (->> (eb-afit-io/read-input in-txt)
                      eb-afit/find-best-pack
                      (reset! r))))
        {:keys [input] :as r} @r]
    (println "!! " resource-name-or-file "costs " (s/trim costs)
             (select-keys r [:percentage-used]) "\n\t"
             (assoc (select-keys r [:packed-volume :packed-number])
                    :total-number (count (:boxes input)))
             (select-keys r [:pallet-variant :layer]))
    (is (some? r) (str "find-best-pack " resource-name-or-file))))

(deftest find-best-dpp-input-test
  (is (= [{:dims [10 10 10]
           :vol 1000
           :n 1
           :pack-dims [10 10 10]
           :pack-coord [0 0 0]}]
         (-> (eb-afit-io/read-input-from-resource "test-dpp00.txt")
             eb-afit/exec-iterations
             :best-pack)))

  (testing "can handle dpp** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "dpp")))]
      (test-find-test-pack-on-resource n))))

(deftest ^:find-best-pack find-best-pack-test
  (when-let [resource-name (System/getenv "input_file")]
    (test-find-test-pack-on-resource resource-name)))

(deftest ^:find-best-pack-large-input find-best-mpp-input-test
  (testing "can handle mpp** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "mpp")))]
      (test-find-test-pack-on-resource n))))

(deftest ^:find-best-pack-large-input find-best-rnd-input-test
  (testing "can handle rnd** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "rnd")))]
      (test-find-test-pack-on-resource n))))
