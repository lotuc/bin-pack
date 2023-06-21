(ns lotuc.binpack.eb-afit-test
  (:use clojure.test)
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as s]
   [lotuc.binpack.eb-afit :as eb-afit]
   [lotuc.binpack.eb-afit-io :as eb-afit-io]
   [lotuc.binpack.eb-afit-io-test :as eb-afit-io-test]))

(deftest apply-found-box-test
  ;; new box will attach to the taller side.

  (testing "smallest-z gap is the only gap"
    (is (= [{:cumx 20 :cumz 20}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 20 :cumz 10}]}
            {:dims [20 10 10]})))
    (is (= [{:cumx 10 :cumz 20} {:cumx 20 :cumz 10}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 20 :cumz 10}]}
            {:dims [10 10 10]}))))

  (testing "smallest-z gap only got right side gaps"
    (testing "right gap higher - attach to right"
      (is (= [{:cumx 5 :cumz 5} {:cumx 10 :cumz 15} {:cumx 20 :cumz 10}]
             (eb-afit/apply-found-box
              {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
              {:dims [5 10 10]}))
          "x not filled - z not evened - higher")
      (is (= [{:cumx 5 :cumz 5} {:cumx 10 :cumz 8} {:cumx 20 :cumz 10}]
             (eb-afit/apply-found-box
              {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
              {:dims [5 10 3]}))
          "x not filled - z not evened - lower")
      (is (= [{:cumx 5 :cumz 5} {:cumx 20 :cumz 10}]
             (eb-afit/apply-found-box
              {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
              {:dims [5 10 5]}))
          "x not filled - z evened"))

    (testing "left gap higher - attach to left"
      (is (= [{:cumx 10 :cumz 10} {:cumx 13 :cumz 15} {:cumx 20 :cumz 5}]
             (eb-afit/apply-found-box
              {:smallest-z 1 :scrap-pad [{:cumx 10 :cumz 10} {:cumx 20 :cumz 5}]}
              {:dims [3 10 10]}))
          "x not filled - z not evened - higher")
      (is (= [{:cumx 10 :cumz 10} {:cumx 13 :cumz 8} {:cumx 20 :cumz 5}]
             (eb-afit/apply-found-box
              {:smallest-z 1 :scrap-pad [{:cumx 10 :cumz 10} {:cumx 20 :cumz 5}]}
              {:dims [3 10 3]}))
          "x not filled - z not evened - lower")
      (is (= [{:cumx 20 :cumz 10}]
             (eb-afit/apply-found-box
              {:smallest-z 1 :scrap-pad [{:cumx 10 :cumz 10} {:cumx 20 :cumz 5}]}
              {:dims [10 10 5]}))
          "x not filled - z evened"))

    (is (= [{:cumx 10 :cumz 15} {:cumx 20 :cumz 10}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [10 10 10]}))
        "x filled - z not evened - higher")
    (is (= [{:cumx 10 :cumz 8} {:cumx 20 :cumz 10}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [10 10 3]}))
        "x filled - z not evened - lower")
    (is (= [{:cumx 20 :cumz 10}]
           (eb-afit/apply-found-box
            {:smallest-z 0 :scrap-pad [{:cumx 10 :cumz 5} {:cumx 20 :cumz 10}]}
            {:dims [10 10 5]}))
        "x filled - z evened"))

  (testing "smallest-z gap only got left side gaps"
    (is (= [{:cumx 5 :cumz 10} {:cumx 7 :cumz 7} {:cumx 10 :cumz 5}]
           (eb-afit/apply-found-box
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [2 10 2]}))
        "x not filled & lower")
    (is (= [{:cumx 5 :cumz 10} {:cumx 7 :cumz 12} {:cumx 10 :cumz 5}]
           (eb-afit/apply-found-box
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [2 10 7]}))
        "x not filled & higher")
    (is (= [{:cumx 7 :cumz 10} {:cumx 10 :cumz 5}]
           (eb-afit/apply-found-box
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [2 10 5]}))
        "x not filled & evened")

    (is (= [{:cumx 5 :cumz 10} {:cumx 10 :cumz 7}]
           (eb-afit/apply-found-box
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [5 10 2]}))
        "x filled & lower")
    (is (= [{:cumx 5 :cumz 10} {:cumx 10 :cumz 12}]
           (eb-afit/apply-found-box
            {:smallest-z 1 :scrap-pad [{:cumx 5 :cumz 10} {:cumx 10 :cumz 5}]}
            {:dims [5 10 7]}))
        "x not filled & higher")
    (is (= [{:cumx 10 :cumz 10}]
           (eb-afit/apply-found-box
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

(deftest find-box-test
  (testing "find-box"

    (eb-afit/find-box
     {:hmx 84 :hy 45 :hmy 104 :hz 96 :hmz 96}
     (:boxes (-> "3d-bin-pack-test/dpp04.txt"
                 eb-afit-io/read-input-from-resource)))

    (eb-afit/find-box
     {:hmx 21 :hy 14 :hmy 104 :hz 7 :hmz 59}
     (-> (:boxes (-> "3d-bin-pack-test/dpp05.txt"
                     eb-afit-io/read-input-from-resource))
         (assoc-in [50 :pack-dims]  1)
         (assoc-in [51 :pack-dims]  1)
         (assoc-in [52 :pack-dims]  1)
         (assoc-in [53 :pack-dims]  1)
         (assoc-in [54 :pack-dims]  1)
         (assoc-in [55 :pack-dims]  1)
         (assoc-in [56 :pack-dims]  1)))

    (is true)))

(deftest list-candit-layers-test
  (testing "List candit layers"
    (-> (eb-afit/list-candit-layers
         (-> "3d-bin-pack-test/dpp04.txt"
             eb-afit-io/read-input-from-resource)
         [84 104 96])
        eb-afit/sort-layers)))

(deftest calc-layer-weight-perf-test
  (testing "calc-layer-weight perf"
    (let [i0 (-> "3d-bin-pack-test/mpp01.txt"
                 eb-afit-io/read-input-from-resource)
          bs (:boxes i0)
          i1 (-> (merge i0 (eb-afit/make-box-xyz-array bs))
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
          i1 (-> (merge i0 (eb-afit/make-box-xyz-array bs))
                 (assoc :box-packed (make-array Boolean/TYPE (count bs))
                        :remain-py 96
                        :pallet [104 96 84]))
          c (* 5 200)]
      (println
       (str "find-layer " c " times: ")
       (with-out-str
         (time (doseq [_ (range c)]
                 (eb-afit/find-layer i1)))))
      (is true))))

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
        r @r]
    (println "!! " resource-name-or-file "costs " (s/trim costs)
             (select-keys r [:percentage-used]))
    (pprint/pprint (-> r
                       (dissoc :input :pack)
                       (assoc :total-number (count (:boxes (:input r))))))
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
