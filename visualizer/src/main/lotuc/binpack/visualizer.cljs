(ns lotuc.binpack.visualizer
  (:require ["@react-three/drei" :refer [OrbitControls PivotControls
                                         GizmoHelper GizmoHelper
                                         Environment Bounds
                                         Grid Edges]]
            ["@react-three/fiber" :refer [Canvas useFrame]]
            ["leva" :refer [useControls]]
            ["react" :refer [useRef]]
            ["react-dom/client" :refer [createRoot]]
            [goog.dom :as gdom]
            [lotuc.binpack.colors :refer [seed-rand-color]]
            [lotuc.binpack.eb-afit :as eb-afit]
            [lotuc.binpack.eb-afit-io :as eb-afit-io]
            [reagent.core :as r]))

;; more test inputs here:
;; https://github.com/wknechtel/3d-bin-pack/tree/master/test
(def sample-input-txt "10, 10, 10
1. 5, 5, 5, 8")

(defn ItemBox [_props]
  (let [hovered (r/atom false)]
    (fn [{:keys [position geometry color opacity]}]
      (let [mesh (useRef)
            _ (useFrame (fn [_state _delta]))]
        [:mesh {:ref mesh
                :position (clj->js position)
                :onPointerOver (fn [_event] (reset! hovered true))
                :onPointerOut (fn [_event] (reset! hovered false))}
         [:boxGeometry {:args (clj->js geometry)}]
         [:meshBasicMaterial
          {:transparent true :opacity opacity :depthTest false :color color}]
         [:> Edges
          [:meshBasicMaterial {:color color :depthTest false}]]]))))

(defn PalletBox [_props]
  (fn [{:keys [position geometry color opacity]}]
    (let [mesh (useRef)
          _ (useFrame (fn [_state _delta]))]
      [:> Bounds {:fit true :clip true :observe true :dmaping 6 :margin 2}
       [:mesh {:ref mesh
               :position (clj->js position)}
        [:boxGeometry {:args (clj->js geometry)}]
        [:meshBasicMaterial {:transparent true :opacity 0}]
        [:> Edges {:scale 1.01 :lineWidth 3}
         [:lineBasicMaterial {:color color}]]]])))

(def default-control-options
  {:gridSize [10 10]
   :boxOpacity {"value" 0.7 "min" 0 "max" 1 "step" 0.1}

   :cellSize {"value" 1 "min" 0 "max" 10 "step" 1}
   :cellThickness {"value" 1 "min" 0 "max" 5 "step" 0.1}
   :cellColor "#6f6f6f"
   :sectionThickness {"value" 1 "min" 0 "max" 5 "step" 0.1}
   :followCamera true
   :infiniteGrid true})

(defn box-visualizer [_props]
  (fn [{:keys [packed-res]}]
    (let [packed-boxes (->> packed-res :pack (filter :pack-dims))
          pallet-variant (:pallet-variant packed-res)

          {:strs [gridSize boxOpacity] :as c}
          (->> default-control-options
               (map (fn [[k v]] [k (clj->js v)]))
               (into {})
               clj->js useControls js->clj)
          grid-props
          (dissoc c "boxOpacity" "gridSize")

          trans-pos
          (fn [pos geo]
            (map + pos (map #(/ % 2) geo)))]
      [:> Canvas
       (when pallet-variant
         [:f> PalletBox {:position (trans-pos [0 0 0] pallet-variant)
                         :geometry pallet-variant
                         :color "red"}])
       [:<>
        (let [rand-color (seed-rand-color 42)]
          (map-indexed (fn [i {:keys [pack-dims pack-coord]}]
                         [:f> ItemBox
                          {:key i
                           :position (trans-pos pack-coord pack-dims)
                           :geometry pack-dims
                           :opacity boxOpacity
                           :color (rand-color)}])
                       packed-boxes))]
       [:> Grid (merge {:position #js [0 0 0] :args gridSize} grid-props)]
       [:> Environment {:preset "city"}]
       [:> OrbitControls {:makeDefault true}]
       [:> PivotControls]
       [:> GizmoHelper {:makeDefault true :alignment "bottom-right" :margin #js [80 80]}
        [:> GizmoHelper {:axisColors #js ["#9d4b4b", "#2f7f4f", "#3b5b9d"] :labelColor "white"}]]])))

(defn app
  []
  (let [input-txt (r/atom sample-input-txt)
        packed-res (r/atom nil)

        find-best-pack
        (fn [txt]
          (let [r (->> txt
                       eb-afit-io/read-input
                       eb-afit/find-best-pack)]
            (reset! packed-res r)))]

    ;; initialize with sample input's packing found.
    (find-best-pack @input-txt)

    (fn []
      [:div {:class "h-screen flex flex-row"}
       ;; Left side input area
       [:div {:class "w-1/4 m-2 h-screen flex flex-col space-y-2"}
        [:div
         [:a {:href "https://github.com/lotuc/bin-pack"
              :target "_blank"
              :class "font-semibold"} "Github lotuc/bin-pack"]]
        [:textarea {:class "h-96 border-solid border-2"
                    :value @input-txt
                    :onChange (fn [e] (reset! input-txt (.. e -target -value)))}]

        [:div
         [:button {:class "p-1 border-solid border-2 border-indigo-600"
                   :onClick (fn [_] (find-best-pack @input-txt))}
          "Find best pack"]]

        [:textarea {:class "h-96 border-solid border-2"
                    :readOnly true
                    :value (some-> (->> @packed-res :pack (filter :pack-dims))
                                   clj->js
                                   (js/JSON.stringify nil 2))}]]
       ;; Right side rendering area
       [:div {:class "w-3/4 h-screen border-solid border-2 border-indigo-600"}
        [:f> box-visualizer {:packed-res @packed-res}]]])))

(defonce root (createRoot (gdom/getElement "root")))

;; https://stackoverflow.com/questions/72389560/how-to-rerender-reagent-ui-with-react-18-and-shadow-cljs-reload
(defn init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (.render root (r/as-element [app])))

(defn ^:dev/after-load start []
  (init))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))
