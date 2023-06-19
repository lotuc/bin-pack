(ns lotuc.binpack.eb-afit-test
  (:use clojure.test)
  (:require
   [lotuc.binpack.eb-afit :as eb-afit]
   [lotuc.binpack.eb-afit-io :as eb-afit-io]))

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

(deftest find-best-test
  (testing "dpp00"
    (is (= [{:dims [10 10 10]
             :vol 1000
             :n 1
             :pack-dims [10 10 10]
             :pack-coord [0 0 0]}]
           (-> (eb-afit-io/read-input-from-resource "test-dpp00.txt")
               eb-afit/exec-iterations
               :best-pack)))))
