(ns lotuc.binpack.visualizer
  (:require ["@react-three/drei" :refer [OrbitControls Bounds useBounds]]
            ["@react-three/fiber" :refer [Canvas useFrame]]
            ["react" :refer [useRef useEffect]]
            ["react-dom/client" :refer [createRoot]]
            [goog.dom :as gdom]
            [lotuc.binpack.colors :refer [seed-rand-color]]
            [lotuc.binpack.eb-afit :as eb-afit]
            [lotuc.binpack.eb-afit-io :as eb-afit-io]
            [reagent.core :as r]))

(def canvas (r/adapt-react-class Canvas))
(def orbit-controls (r/adapt-react-class OrbitControls))
(def bounds (r/adapt-react-class Bounds))

(def sample-input-txt "10, 10, 10
1. 5, 5, 5, 8")

(defn BoxMesh [_props]
  (let [hovered (r/atom false)
        active (r/atom false)]
    (fn [{:keys [position geometry color]}]
      (let [mesh (useRef)
            _ (useFrame (fn [_state _delta]))

            b (useBounds)
            _ (useEffect (fn [] (.. b refresh clip fit) (fn []))
                         #js [])]
        [:mesh {:ref mesh
                :position (clj->js position)
                :scale (if @active 1.5 1)
                ;; :onClick (fn [_event] (swap! active not))
                :onPointerOver (fn [_event] (reset! hovered true))
                :onPointerOut (fn [_event] (reset! hovered false))}
         [:boxGeometry {:args (clj->js geometry)}]
         [:meshStandardMaterial {:color color}]]))))

(def light-position-v 100)

(defn main-panel [_props]
  (fn [{:keys [input-txt]}]
    [canvas
     [:ambientLight {:intensity 0.5}]
     [:spotLight {:position (clj->js [light-position-v
                                      light-position-v
                                      light-position-v])
                  :angle 0.15
                  :penumbra 1}]
     [:pointLight {:position (clj->js [light-position-v
                                       light-position-v
                                       light-position-v])}]
     [bounds
      [:<>
       (let [rand-color (seed-rand-color 42)]
         (->> input-txt
              eb-afit-io/read-input
              eb-afit/find-best-pack
              :pack
              (filter :pack-dims)
              (map-indexed (fn [i {:keys [pack-dims pack-coord]}]
                             [:f> BoxMesh {:key i
                                           :position pack-coord
                                           :geometry pack-dims
                                           :color (rand-color)}]))))]]
     [orbit-controls]]))

(defn app
  []
  (let [packed-txt (r/atom sample-input-txt)
        input-txt (r/atom sample-input-txt)]
    (fn []
      [:div {:class "h-screen flex flex-row"}
       [:div {:class "w-1/4 h-screen flex flex-col"}
        [:button
         {:onClick (fn [_]
                     (reset! packed-txt sample-input-txt)
                     (reset! input-txt sample-input-txt))}
         "Reset Input"]
        [:button
         {:onClick (fn [_]
                     (reset! packed-txt @input-txt))}
         "Render Input"]
        [:textarea {:class "h-96 border-solid border-2 border-indigo-600 p-2"
                    :value @input-txt
                    :onChange (fn [e] (reset! input-txt (.. e -target -value)))}]]
       [:div {:class "w-3/4 h-screen border-solid border-2 border-indigo-600"}
        [:f> main-panel {:input-txt @packed-txt}]]])))

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
