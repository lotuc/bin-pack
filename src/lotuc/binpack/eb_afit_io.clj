(ns lotuc.binpack.eb-afit-io
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(declare input-box-list)

(defn read-input
  "Read the test data format defined in in paper
  https://scholar.afit.edu/etd/4563/.

  Some of the test data can be found here at
  https://github.com/wknechtel/3d-bin-pack/tree/master/test."
  [in-txt]
  (-> in-txt s/split-lines input-box-list))

(defn read-input-from-resource
  "Read the test data format defined in in paper
  https://scholar.afit.edu/etd/4563/.

  Some of the test data can be found here at
  https://github.com/wknechtel/3d-bin-pack/tree/master/test."
  [resource-name]
  (-> resource-name io/resource slurp read-input))

(defn input-box-list-read-line [line]
  (let [[_ _lbl dim0 dim1 dim2 boxn]
        (re-find #"(.+)\.\s+(.+),\s+(.+),\s+(.+),\s+(.+)" line)
        n (parse-long boxn)
        dim0 (parse-long dim0)
        dim1 (parse-long dim1)
        dim2 (parse-long dim2)
        box {:dims [dim0 dim1 dim2]
             :vol  (* dim0 dim1 dim2)
             :n n}]
    [n box]))

(defn input-box-list [lines]
  (let [handle-pallet-line
        (fn [line]
          (let [[xx yy zz]
                (->> (s/split line #",")
                     (map s/trim)
                     (map parse-long))]
            {:pallet-volume (* xx yy zz)
             :pallet-dims [xx yy zz]}))

        handle-box-line
        (fn [[boxes box-vol] line]
          (let [[n box] (input-box-list-read-line line)]
            [(reduce (fn [boxes' _] (conj boxes' box)) boxes (range n))
             (+ box-vol (* n (:vol box)))]))

        [boxes box-vol]
        (reduce handle-box-line [[] 0] (rest lines))]
    (merge
     (handle-pallet-line (first lines))
     {:boxes boxes :box-volume box-vol})))
