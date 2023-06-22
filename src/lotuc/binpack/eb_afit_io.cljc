(ns lotuc.binpack.eb-afit-io
  (:require
   #?(:clj [clojure.java.io :as io])
   [clojure.string :as s]))

(declare parse-eb-afit-input)
(declare parse-visualdot)

(defn read-input
  "Read the test data format defined in in paper
  https://scholar.afit.edu/etd/4563/.

  Some of the test data can be found here at
  https://github.com/wknechtel/3d-bin-pack/tree/master/test.

  Input:
  ```
  104, 96, 84
  1. 70, 104, 24, 1
  2. 14, 104, 48, 2
  ```

  Output:
  ```
  {:boxes
   [{:label \"1\", :dims [70 104 24], :vol 174720, :n 1}
    {:label \"2\", :dims [14 104 48], :vol 69888, :n 2}
    {:label \"2\", :dims [14 104 48], :vol 69888, :n 2}],
   :box-volume 454272,
   :pallet-volume 838656,
   :pallet-dims [104 96 84]}
  ```
  "
  [in-txt]
  (->> in-txt s/split-lines parse-eb-afit-input))

(defn read-visualdot
  "Read the ref implementation's output visualdot file.

  ```
   84  104   96
    0    0    0   70   45   24
   70    0    0   14   40   48
    0    0   24   70   45   24
  ```

  ```
  {:pack
   [{:pack-coord [0 0 0], :pack-dims [0 0 0]}
    {:pack-coord [70 0 0], :pack-dims [70 0 0]}
    {:pack-coord [0 0 24], :pack-dims [0 0 24]}],
  :pallet [84 104 96]}
  ```
  "
  [in-txt]
  (->> in-txt s/split-lines parse-visualdot))

(defn gen-visualdot
  [{:keys [pallet pack]}]
  (with-out-str
    (println (s/join " " pallet))
    (doseq [{:keys [pack-coord pack-dims]} pack]
      (println (s/join " " (concat pack-coord pack-dims))))))

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

(def regexp-visualdot-box-line
  #"(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)")

(def regexp-visualdot-pallet-line
  #"(\d+)\s+(\d+)\s+(\d+)")

(defn- assert-line-valid [valid? {:keys [line txt] :as x} desc]
  (when-not valid? (throw (ex-info (str "invalid " desc) x))))

(defn parse-box-line [{:keys [line txt] :as x}]
  (let [assert-valid
        (fn [valid?] (assert-line-valid valid? x "box info"))

        r (re-find regexp-box-line txt)

        _ (assert-valid (some? r))
        [_ lbl dim0 dim1 dim2 boxn] r

        lbl (-> lbl (s/replace #"\.$\s*$" "") s/trim)
        n (parse-long boxn)
        dims (->> [dim0 dim1 dim2] (map parse-long) (into []))]

    (assert-valid (every? some? (conj dims n)))
    {:label lbl :dims dims :vol  (apply * dims) :n n}))

(defn parse-pallet-line [{:keys [line txt] :as x}]
  (let [assert-valid
        (fn [valid?] (assert-line-valid valid? x "pallet info"))

        r (re-find regexp-pallet-line txt)
        _ (assert-valid (some? r))

        dims (->> (rest r) (map parse-long) (into []))]
    {:pallet-volume (apply * dims)
     :pallet-dims dims}))

(defn parse-eb-afit-input [lines]
  (let [parse-fn
        (fn [{:keys [pallet-volume pallet-dims boxes box-volume] :as s}
             {:keys [line txt] :as x}]
          (cond
            (empty? txt) s
            (not pallet-dims) (merge s (parse-pallet-line x))
            :else (let [{:keys [n vol] :as box} (parse-box-line x)]
                    (loop [i 0 s s]
                      (if (< i n)
                        (recur (inc i) (-> s
                                           (update :boxes conj box)
                                           (update :box-volume + (* n vol))))
                        s)))))]
    (->> lines
         (map s/trim)
         (map-indexed (fn [i txt] {:line (inc i) :txt txt}))
         (reduce parse-fn {:boxes [] :box-volume 0}))))

(defn parse-visualdot [lines]
  (let [parse-fn
        (fn [{:keys [pallet pack] :as s}
             {:keys [line txt] :as x}]
          (cond
            (empty? txt) s
            (not pallet)
            (let [[_ & r] (re-find regexp-visualdot-pallet-line txt)]
              (assert-line-valid (every? some? r) x "pallet")
              (assoc s :pallet (->> r (map parse-long) (into []))))
            :else
            (let [[_ & r] (re-find regexp-visualdot-box-line txt)]
              (assert-line-valid (every? some? r) x "box")
              (let [vs (->> r (map parse-long) (into []))]
                (update s :pack conj
                        {:pack-coord (subvec vs 0 3)
                         :pack-dims (subvec vs 3)})))))]
    (->> lines
         (map s/trim)
         (map-indexed (fn [i txt] {:line (inc i) :txt txt}))
         (reduce parse-fn {:pack []}))))
