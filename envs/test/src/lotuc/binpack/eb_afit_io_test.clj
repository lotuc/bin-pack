(ns lotuc.binpack.eb-afit-io-test
  (:use clojure.test)
  (:require [lotuc.binpack.eb-afit-io :as eb-afit-io]))

(def test-resources
  ["3d-bin-pack-test/dpp01.txt"
   "3d-bin-pack-test/dpp02.txt"
   "3d-bin-pack-test/dpp03.txt"
   "3d-bin-pack-test/dpp04.txt"
   "3d-bin-pack-test/dpp05.txt"
   "3d-bin-pack-test/dpp06.txt"
   "3d-bin-pack-test/mpp01.txt"
   "3d-bin-pack-test/mpp02.txt"
   "3d-bin-pack-test/mpp03.txt"
   "3d-bin-pack-test/mpp04.txt"
   "3d-bin-pack-test/mpp05.txt"
   "3d-bin-pack-test/rnd01.txt"
   "3d-bin-pack-test/rnd02.txt"
   "3d-bin-pack-test/rnd03.txt"
   "3d-bin-pack-test/rnd04.txt"
   "3d-bin-pack-test/rnd05.txt"])

(deftest read-input-test-test
  (testing "can parse all test inputs."
    (doseq [n test-resources]
      (let [{:keys [pallet-volume pallet-dims boxes box-volume] :as r}
            (eb-afit-io/read-input-from-resource n)]
        (is (and (some? r)
                 (some? pallet-volume)
                 (some? pallet-dims)
                 (some? boxes)
                 (some? box-volume))
            (str "read resource " n))))))

(deftest read-visualdot-test
  (testing "read visualdot txt"
    (is (=
         {:pack
          [{:pack-coord [0 0 0], :pack-dims [70 45 24]}
           {:pack-coord [70 0 0], :pack-dims [14 40 48]}
           {:pack-coord [0 0 24], :pack-dims [70 45 24]}],
          :pallet [84 104 96]}
         (eb-afit-io/read-visualdot "   84  104   96
    0    0    0   70   45   24
   70    0    0   14   40   48
    0    0   24   70   45   24")))))
