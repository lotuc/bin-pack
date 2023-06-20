(ns lotuc.binpack.eb-afit-test
  (:use clojure.test)
  (:require
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
      (println "testing" n)
      (println
       "!! testing" n "costs"
       (with-out-str
         (time
          (is (some? (-> (eb-afit-io/read-input-from-resource n)
                         eb-afit/exec-iterations
                         :best-pack))
              (str "find-best-pack for " n))))))))

(deftest ^:find-best-large-input find-best-mpp-input-test
  (testing "can handle mpp** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "mpp")))]
      (println "testing" n)
      (println
       "!! testing" n "costs"
       (with-out-str
         (time
          (is (some? (-> (eb-afit-io/read-input-from-resource n)
                         eb-afit/exec-iterations
                         :best-pack))
              (str "find-best-pack for " n))))))))

(deftest ^:find-best-large-input find-best-rnd-input-test
  (testing "can handle rnd** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "rnd")))]
      (println "testing" n)
      (println
       "!! testing" n "costs"
       (with-out-str
         (time
          (is (some? (-> (eb-afit-io/read-input-from-resource n)
                         eb-afit/exec-iterations
                         :best-pack))
              (str "find-best-pack for " n))))))))
