(ns lotuc.binpack.eb-afit-test
  (:use clojure.test)
  (:require [lotuc.binpack.eb-afit :as t]))

(deftest apply-found-box-test
  (testing "smallest-z gap is the only gap"
    (is (= {:scrap-pad [{:cumx 20 :cumz 20}]}
           (t/apply-found-box
            {:scrap-pad [{:cumx 20 :cumz 10}]
             :smallest-z 0}
            {:dims [20 10 10]}))))

  (testing "smallest-z gap has gap on right side."
    (is (= {:scrap-pad [{:cumx 20, :cumz 10}]}
           (t/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [5 11 5]}))
        "x direction is filled -> merge to right")
    (is (= {:scrap-pad [{:cumx 5, :cumz 8} {:cumx 20, :cumz 10}]}
           (t/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [5 11 3]}))
        "x direction is filled")
    (is (= {:scrap-pad [{:cumx 4, :cumz 8} {:cumx 5, :cumz 5} {:cumx 20, :cumz 10}]}
           (t/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [4 11 3]}))
        "x direction is not filled"))

  ;; todo
  (testing "smallest-z gap has gap on left side.")

  ;; todo
  (testing "smallest-z gap has gap on both side."))
