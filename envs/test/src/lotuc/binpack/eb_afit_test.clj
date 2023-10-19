(ns lotuc.binpack.eb-afit-test
  (:use clojure.test)
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as s]
   [lotuc.binpack.eb-afit :as eb-afit]
   [lotuc.binpack.eb-afit-io :as eb-afit-io]
   [lotuc.binpack.eb-afit-io-test :as eb-afit-io-test]))

(def use-pmap (= (System/getenv "USE_PMAP") "true"))

(defn- find-best-pack* [input & opts]
  (eb-afit/find-best-pack input (merge opts {:use-pmap use-pmap})))

(defn- apply-found-box-get-scrap-pad
  [{:keys [scrap-pad smallest-z]} box]
  (:scrap-pad
   (eb-afit/apply-found-box
    {:smallest-z smallest-z :scrap-pad scrap-pad
     :boxes [{:vol 1}]
     :box-packed (make-array java.lang.Boolean/TYPE 1)}
    (assoc box :index 0)
    {})))

(deftest apply-found-box-test
  ;; new box will attach to the taller side.
  (testing "smallest-z gap is the only gap"
    (is (= [{:cumx 20 :cumz 20}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 0 :scrap-pad [{:cumx 20 :cumz 10}]}
            {:dims [20 10 10]})))
    (is (= [{:cumx 10 :cumz 20} {:cumx 20 :cumz 10}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 0 :scrap-pad [{:cumx 20 :cumz 10}]}
            {:dims [10 10 10]}))))

  (testing "smallest-z gap only got right side gaps"
    (testing "right gap higher - attach to right"
      (is (= [{:cumx 5 :cumz 5} {:cumx 10 :cumz 15} {:cumx 20 :cumz 10}]
             (apply-found-box-get-scrap-pad
              {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
              {:dims [5 10 10]}))
          "x not filled - z not evened - higher")
      (is (= [{:cumx 5 :cumz 5} {:cumx 10 :cumz 8} {:cumx 20 :cumz 10}]
             (apply-found-box-get-scrap-pad
              {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
              {:dims [5 10 3]}))
          "x not filled - z not evened - lower")
      (is (= [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]
             (apply-found-box-get-scrap-pad
              {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
              {:dims [5 10 5]}))
          "x not filled - z evened"))

    (testing "left gap higher - attach to left"
      (is (= [{:cumx 10 :cumz 10} {:cumx 13 :cumz 15} {:cumx 20 :cumz 5}]
             (apply-found-box-get-scrap-pad
              {:smallest-z 1 :scrap-pad [{:cumx 10 :cumz 10} {:cumx 20 :cumz 5}]}
              {:dims [3 10 10]}))
          "x not filled - z not evened - higher")
      (is (= [{:cumx 10 :cumz 10} {:cumx 13 :cumz 8} {:cumx 20 :cumz 5}]
             (apply-found-box-get-scrap-pad
              {:smallest-z 1 :scrap-pad [{:cumx 10 :cumz 10} {:cumx 20 :cumz 5}]}
              {:dims [3 10 3]}))
          "x not filled - z not evened - lower")
      (is (= [{:cumx 20 :cumz 10}]
             (apply-found-box-get-scrap-pad
              {:smallest-z 1 :scrap-pad [{:cumx 10 :cumz 10} {:cumx 20 :cumz 5}]}
              {:dims [10 10 5]}))
          "x not filled - z evened"))

    (is (= [{:cumx 10 :cumz 15} {:cumx 20 :cumz 10}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [10 10 10]}))
        "x filled - z not evened - higher")
    (is (= [{:cumx 10 :cumz 8} {:cumx 20 :cumz 10}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [10 10 3]}))
        "x filled - z not evened - lower")
    (is (= [{:cumx 20 :cumz 10}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [10 10 5]}))
        "x filled - z evened"))

  (testing "smallest-z gap only got left side gaps"
    (is (= [{:cumx 5 :cumz 10} {:cumx 7 :cumz 7} {:cumx 10 :cumz 5}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [2 10 2]}))
        "x not filled & lower")
    (is (= [{:cumx 5 :cumz 10} {:cumx 7 :cumz 12} {:cumx 10 :cumz 5}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [2 10 7]}))
        "x not filled & higher")
    (is (= [{:cumx 7 :cumz 10} {:cumx 10 :cumz 5}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [2 10 5]}))
        "x not filled & evened")

    (is (= [{:cumx 5 :cumz 10} {:cumx 10 :cumz 7}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [5 10 2]}))
        "x filled & lower")
    (is (= [{:cumx 5 :cumz 10} {:cumx 10 :cumz 12}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [5 10 7]}))
        "x not filled & higher")
    (is (= [{:cumx 10 :cumz 10}]
           (apply-found-box-get-scrap-pad
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [5 10 5]}))
        "x not filled & evened")))

(deftest find-smallest-z-test
  (testing "find smallest-z"
    (is (= nil (eb-afit/find-smallest-z [])))
    (is (= 0 (eb-afit/find-smallest-z [{:cumx 5 :cumz 10}])))
    (is (= 1 (eb-afit/find-smallest-z [{:cumx 5 :cumz 10}
                                       {:cumx 7 :cumz 9}])))
    (is (= 2 (eb-afit/find-smallest-z [{:cumx 5 :cumz 10}
                                       {:cumx 7 :cumz 9}
                                       {:cumx 8 :cumz 8}])))
    (is (= 2 (eb-afit/find-smallest-z [{:cumx 5 :cumz 10}
                                       {:cumx 7 :cumz 9}
                                       {:cumx 8 :cumz 8}
                                       {:cumx 8 :cumz 9}])))))

(deftest get-smallest-z-gap-geo-test
  (testing "find smallest-z gap's geo"
    (is (= {:hmx 10 :hy 10 :hmy 20 :hz 15 :hmz 15}
           (eb-afit/get-smallest-z-gap-geo
            {:scrap-pad [{:cumx 10 :cumz 5}]
             :layer-thickness 10
             :remain-pz 20
             :remain-py 20
             :smallest-z 0})))
    (is (= {:hmx 5 :hy 10 :hmy 20 :hz 3 :hmz 15}
           (eb-afit/get-smallest-z-gap-geo
            {:scrap-pad [{:cumx 5 :cumz 8} {:cumx 10 :cumz 5}]
             :layer-thickness 10
             :remain-pz 20
             :remain-py 20
             :smallest-z 1})))
    (is (= {:hmx 10 :hy 10 :hmy 20 :hz 2 :hmz 15}
           (eb-afit/get-smallest-z-gap-geo
            {:scrap-pad [{:cumx 10 :cumz 5} {:cumx 15 :cumz 7}]
             :layer-thickness 10
             :remain-pz 20
             :remain-py 20
             :smallest-z 0})))))

(deftest apply-ignore-gap-test
  (testing "the gap to ignore is in middle"
    (is (= [{:cumx 104, :cumz 84}]
           (eb-afit/apply-ignore-gap
            {:scrap-pad [{:cumx 30, :cumz 84}
                         {:cumx 36, :cumz 77}
                         {:cumx 104, :cumz 84}]
             :smallest-z 1}))
        "pre & pos's cumz is equal")
    (is (= [{:cumx 20 :cumz 30} {:cumx 104, :cumz 84} {:cumx 20 :cumz 30}]
           (eb-afit/apply-ignore-gap
            {:scrap-pad [{:cumx 20 :cumz 30}
                         {:cumx 30, :cumz 84}
                         {:cumx 36, :cumz 77}
                         {:cumx 104, :cumz 84}
                         {:cumx 20 :cumz 30}]
             :smallest-z 2}))
        "pre & pos's cumz is equal")
    (is (= [{:cumx 30, :cumz 90}
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

(deftest list-candit-layer-thickness-with-weight-test
  (testing "List candit layers"
    (is (some? (-> (eb-afit/list-candit-layer-thickness-with-weight
                    (-> "3d-bin-pack-test/dpp04.txt"
                        eb-afit-io/read-input-from-resource
                        (as-> $ (merge $ (eb-afit/vectorize-box-coords (:boxes $)))))
                    [84 104 96]))))))

(deftest calc-layer-weight-perf-test
  (testing "calc-layer-weight perf"
    (let [i0 (-> "3d-bin-pack-test/mpp01.txt"
                 eb-afit-io/read-input-from-resource)
          bs (:boxes i0)
          i1 (-> (merge i0 (eb-afit/vectorize-box-coords bs))
                 (assoc :box-packed (make-array Boolean/TYPE (count bs))))
          c 300000]
      (println
       (str "calc-layer " c " times: ")
       (with-out-str
         (time (doseq [_ (range c)]
                 (eb-afit/calc-layer-weight i1 10 14)))))
      (is true))))

(deftest find-layer-perf-test
  (testing "find-layer perf"
    (let [i0 (-> "3d-bin-pack-test/mpp01.txt"
                 eb-afit-io/read-input-from-resource)
          bs (:boxes i0)
          i1 (-> (merge i0 (eb-afit/vectorize-box-coords bs))
                 (assoc :box-packed (make-array Boolean/TYPE (count bs))
                        :remain-py 96
                        :pallet [104 96 84]))
          c (* 5 200)]
      (println
       (str "find-layer " c " times: ")
       (with-out-str
         (time (doseq [_ (range c)]
                 (eb-afit/find-layer-thickness i1)))))
      (is true))))

(def expected-results
  (-> (io/resource "expected-pack-res.edn") slurp read-string))

(defn- check-resource-best-result
  [resource-name-or-file {:keys [percentage-used] :as r}]
  (when-let [expected (get expected-results resource-name-or-file)]
    (is (< (abs (- (:percentage-used expected) percentage-used)) 0.00001)
        (str "expected " (:percentage-used expected) " vs. " percentage-used))
    (when-not use-pmap
      (is (= (select-keys r [:packed-volume :packed-number :pallet-variant
                             :first-layer-thickness])
             (dissoc expected :percentage-used))))))

(defn- test-find-test-pack-on-resource [resource-name-or-file]
  (println "!! find-best-pack " resource-name-or-file)
  (let [in-txt (if (.exists (io/file resource-name-or-file))
                 (slurp resource-name-or-file)
                 (slurp (io/resource resource-name-or-file)))
        input (eb-afit-io/read-input in-txt)
        r (atom nil)
        costs (with-out-str (time (reset! r (find-best-pack* input))))
        r @r
        r' (-> r
               (dissoc :pack :unpacked)
               (assoc :total-number (count (:boxes input)))
               (assoc :packed-number (count (:pack r))))]
    (println ">>" resource-name-or-file "(use-pmap=true) costs " (s/trim costs)
             (s/replace (str "\n" (with-out-str (pprint/pprint r')))
                        #"\n" "\n   "))
    (is (some? r) (str "find-best-pack " resource-name-or-file))
    (check-resource-best-result resource-name-or-file r')))

(deftest find-best-dpp-input-test
  (testing "can handle dpp** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "dpp")))]
      (testing (str ": handle " n)
        (test-find-test-pack-on-resource n)))))

(deftest ^:find-best-pack find-best-pack-test
  (when-let [resource-name (System/getenv "input_file")]
    (test-find-test-pack-on-resource resource-name)))

(deftest ^:find-best-pack-large-input find-best-mpp-input-test
  (testing "can handle mpp** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "mpp")))]
      (testing (str ": handle " n)
        (test-find-test-pack-on-resource n)))))

(deftest ^:find-best-pack-large-input find-best-rnd-input-test
  (testing "can handle rnd** resources"
    (doseq [n (->> eb-afit-io-test/test-resources
                   (filter #(s/includes? % "rnd")))]
      (testing (str ": handle " n)
        (test-find-test-pack-on-resource n)))))

(deftest find-best-pack-not-found-test
  (testing "we do not found a packing scheme"
    (is (nil? (find-best-pack*
               {:pallet-dims [80 80 80]
                :boxes [{:dims [90 90 90] :n 1}]})))))
