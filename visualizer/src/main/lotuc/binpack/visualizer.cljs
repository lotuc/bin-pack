(ns lotuc.binpack.visualizer
  (:require ["@react-three/drei" :refer [OrbitControls]]
            ["@react-three/fiber" :refer [Canvas useFrame]]
            ["react" :refer [useRef]]
            ["react-dom/client" :refer [createRoot]]
            [goog.dom :as gdom]
            [reagent.core :as r]))

(def canvas (r/adapt-react-class Canvas))
(def orbit-controls (r/adapt-react-class OrbitControls))

(defn BoxMesh [_props]
  (let [hovered (r/atom false)
        active (r/atom false)]
    (fn [{:keys [position]}]
      (let [mesh (useRef)
            _ (useFrame (fn [_state delta]
                          (set! (.. mesh -current -rotation -x)
                                (+ (.. mesh -current -rotation -x) delta))))]
        [:mesh {:ref mesh
                :position (clj->js position)
                :scale (if @active 1.5 1)
                :onClick (fn [_event] (swap! active not))
                :onPointerOver (fn [_event] (reset! hovered true))
                :onPointerOut (fn [_event] (reset! hovered false))}
         [:boxGeometry {:args (clj->js [1 1 1])}]
         [:meshStandardMaterial {:color (if @hovered "hotpink" "orange")}]]))))

(defn main-panel []
  (r/create-class
   {:component-did-mount
    (fn [])
    :reagent-render
    (fn []
      [canvas
       [:ambientLight {:intensity 0.5}]
       [:spotLight {:position (clj->js [10 10 10])
                    :angle 0.15
                    :penumbra 1}]
       [:pointLight {:position (clj->js [-10 -10 -10])}]
       [:f> BoxMesh {:position [-1.2 0 0]}]
       [:f> BoxMesh {:position [1.2 0 0]}]
       [orbit-controls]])}))

(defn app
  []
  [:div {:class "h-screen"}
   [main-panel]])

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
