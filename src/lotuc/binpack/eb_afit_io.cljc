(ns lotuc.binpack.eb-afit-io
  (:require
   #?(:clj [clojure.java.io :as io])
   [clojure.string :as s]))

(declare input-box-list)

(defn read-input
  "Read the test data format defined in in paper
  https://scholar.afit.edu/etd/4563/.

  Some of the test data can be found here at
  https://github.com/wknechtel/3d-bin-pack/tree/master/test."
  [in-txt]
  (->> in-txt s/split-lines (filter seq) input-box-list))

#?(:clj
   (defn read-input-from-resource
     "Read the test data format defined in in paper
  https://scholar.afit.edu/etd/4563/.

  Some of the test data can be found here at
  https://github.com/wknechtel/3d-bin-pack/tree/master/test."
     [resource-name]
     (-> resource-name io/resource slurp read-input)))

(def regexp-box-line
  #"(.+?)\s+(\d+)\s*,?\s+(\d+)\s*,?\s+(\d+)\s*,?\s+(\d+)\s*$")

(def regexp-pallet-line
  #"(\d+)\s*,?\s+(\d+)\s*,?\s+(\d+)\s*$")

(defn- assert-line-valid [valid? line desc]
  (when-not valid?
    (throw (ex-info (str "invalid " desc) {:line line}))))

(defn parse-box-line [line]
  (let [r (re-find regexp-box-line line)

        _ (assert-line-valid (some? r) line "box info")
        [_ _lbl dim0 dim1 dim2 boxn] r

        n (parse-long boxn)
        dim0 (parse-long dim0)
        dim1 (parse-long dim1)
        dim2 (parse-long dim2)]

    (assert-line-valid (every? some? [n dim0 dim1 dim2]) line "box info")
    {:dims [dim0 dim1 dim2] :vol  (* dim0 dim1 dim2) :n n}))

(defn parse-pallet-line [line]
  (let [r (re-find regexp-pallet-line line)
        _ (assert-line-valid (some? r) line "pallet info")
        [_ x y z] r

        xx (parse-long x)
        yy (parse-long y)
        zz (parse-long z)]
    (assert-line-valid (every? some? [xx yy zz]) line "pallet info")
    {:pallet-volume (* xx yy zz)
     :pallet-dims [xx yy zz]}))

(defn input-box-list [lines]
  (let [handle-box-line
        (fn [[boxes box-vol] line]
          (let [{:keys [n] :as box} (parse-box-line line)]
            [(reduce (fn [boxes' _] (conj boxes' box)) boxes (range n))
             (+ box-vol (* n (:vol box)))]))

        [boxes box-vol]
        (reduce handle-box-line [[] 0] (rest lines))]
    (merge
     (parse-pallet-line (first lines))
     {:boxes boxes :box-volume box-vol})))
