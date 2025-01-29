(ns de.levering-it.electric.three.example.world3d
  (:require [hyperfiddle.electric3 :as e]
            [de.levering-it.electric.three :as three]
            [de.levering-it.electric.three.bindings :as tb]
            [de.levering-it.electric.three.utils :as tu]
            [de.levering-it.electric.three.impl.experimental.physics :refer [move-entity]]
            [de.levering-it.electric.three.controls.orbit :refer [OrbitControls]]
            [de.levering-it.electric.three.controls.first-person :refer [FirstPersonControls]]
            [de.levering-it.electric.three.dom :as tdom]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms3 :refer [Checkbox]]
            [clojure.string :as s]
            #?(:cljs [goog.object :as gobj])))

(def player-state
  {:x     -10.0
   :y     0.0
   :z     0.0
   :eyes-height 1.75
   :vx    0.0
   :vy    0.0
   :vz    0.0
   :yaw   0.0
   :pitch 0.0
   :roll  0.0

   ;; Physical parameters
   :mass     80.0        ;; 80 kg
   :mi_yaw   1.0
   :mi_pitch 1.0
   :mi_roll  1.0
   :look-at [1 0 0]})

(def opts
  {:force             1000.0   ;; N (Newtons)
   :torque_yaw        5.0
   :torque_pitch      0.0
   :torque_roll       0.0
   :friction          0.5
   :G                 1.6 #_9.81
   :max-speed         5.0
   :initial-jump-speed 3.0})

(comment
  (cond-> 1
    false inc
    true inc)
  (-> player-state
    (move-entity 5 :move-forward opts)
    (move-entity 5 :none opts)
    #_(move-entity 10 :move-forward opts)))

#?(:cljs
   (defn setPosition [^js m rows columns]
     (doseq [j (range rows)]
       (doseq [i (range columns)]
         (let [matrix (three/Matrix4)
               x (* 2 (- (/ columns 2) i))
               y 0.5
               z (* 2 (- (/ rows 2) j))]
           (.setPosition matrix x y z)
           (.setMatrixAt  m (+ (* j columns) i) matrix))))))

(e/defn Example []
  (e/client
    (dom/div
      (println "GPU?" (gobj/get js/navigator "gpu"))
      (let [key-state (binding [dom/node (.-body js/document)] (tu/KeyState))
            cb (dom/div
                 (dom/text "wireframe:")
                 (Checkbox false))
            fp?! (atom false)
            fp? (e/watch fp?!)
            fp (dom/div
                 (dom/text "first-person:")
                 (Checkbox fp?))
            fc?! (atom false)
            fc? (e/watch fc?!)
            fc (dom/div
                 (dom/text "fullscreen:")
                 (Checkbox fc?))
            q? (get key-state "q")
            p? (get key-state "p")]
        cb fp fc ;prevent unloading
        (reset! fc?! (tu/FullscreenControl fc))
        (reset! fp?! fp)
        (when q? (swap! fp?! not))
        (when p? (swap! fc?! not))
        (dom/div
          (dom/props {:style {:height "100vh" :width "100%" :position "absolute" :top 0 :left 0 :z-index -1}})
          (three/WebGLRenderer
            (let [up? (get key-state "ArrowUp")
                  down? (get key-state "ArrowDown")
                  left? (get key-state "ArrowLeft")
                  right? (get key-state "ArrowRight")
                  shift? (get key-state "Shift")
                  spacebar? (get key-state " ")
                  state! (atom player-state)
                  state (e/watch state!)
                  x (:x state)
                  y (:y state)
                  z (:z state)
                  yaw (:yaw state)
                  eyes-height (+ y (:eyes-height state))
                  [lx ly lz] (:look-at state)
                  t (e/System-time-ms)
                  pointer-lock? (tu/PointerLock?)]
              (reset! fp?! pointer-lock?)
              (e/client
                (let [actions (cond-> #{}
                                spacebar? (conj :jump)
                                (and shift? left?) (conj :move-left)
                                (and shift? right?) (conj :move-right)
                                (and (not shift?) left?) (conj :turn-left)
                                (and (not shift?) right?) (conj :turn-right)
                                up? (conj :move-forward)
                                down? (conj :move-backward))]
                  ((tu/with-t->dt (fn [dt state!]
                                    (swap! state! move-entity dt actions opts))) t state!)))
              (let [c  (three/PerspectiveCamera 75 1 0.1 2000
                         ;set aspect via props, so that camera does not get recreated during screen size change
                         (case (three/props {:position {:x (e/snapshot (- x 10)) :y (e/snapshot eyes-height) :z (e/snapshot z)}
                                             :aspect (/ tb/view-port-width tb/view-port-height)})
                           (.lookAt tb/node (e/snapshot x) (e/snapshot y) (e/snapshot z))))
                    pos! (atom {:x (e/snapshot (- x 10)) :y (e/snapshot eyes-height) :z (e/snapshot z)})
                    pos (e/watch pos!)
                    box (three/BoxGeometry 1 1 1)
                    box2 (three/BoxGeometry 1 1 1)
                    material (three/MeshBasicMaterial nil
                               (three/props {:wireframe false
                                             :color (if cb (three/Color 1 0 0) (three/Color 0 1 0))}))
                    skybox (three/BoxGeometry 1000 1000 1000)
                    skybox-mat (three/MeshBasicMaterial nil
                                 (three/props {:color (three/Color 0.4 0.6 0.9), :side three/backSide}))
                    texture (.load
                              (three/TextureLoader)
                              "stone2.jpg")
                    ground-texture  (.load (three/TextureLoader) "grass2.jpg"
                                      (fn [t]
                                        (set! (.-wrapS t) three/repeatWrapping)
                                        (set! (.-wrapT t) three/repeatWrapping)
                                        (.set (.-repeat t) 100 100)))
                    ground (three/PlaneGeometry 1000 1000 1 1)
                    ground-mat (three/MeshPhongMaterial nil
                                 (three/props {:shininess 100
                                               :map ground-texture
                                               :specular (three/Color 0.5 0.5 0.5)}))
                    phong-material (three/MeshPhongMaterial nil
                                     (three/props {:shininess 100
                                                   :wireframe cb
                                                   :map texture
                                                   :specular (three/Color 0.5 0.5 0.5)}))

                    render-target (three/WebGLCubeRenderTarget 512 (clj->js {:generateMipmaps true
                                                                             :minFilter three/linearMipmapLinearFilter}))
                    cube-x 5
                    cube-z 0
                    cube-camera (three/CubeCamera 0.2 2000 render-target
                                  (three/props {:position {:x cube-x :y eyes-height :z cube-z}}))
                    cube-material (three/MeshBasicMaterial nil
                                    (three/props {:envMap (.-texture render-target)
                                                  :reflectivity 0.5}))
                    new-pos (if fp
                              (FirstPersonControls c {})
                              (OrbitControls c {:x x :y y :z z} {}))
                    crosshair-material (three/LineBasicMaterial (clj->js {})
                                         (three/props {:color (three/Color 1 0.0 0.0)}))
                    crosshair-geometry (three/BufferGeometry)
                    vertices (e/client (js/Float32Array. (clj->js [-0.02 0 0  0.02 0 0  0 -0.02 0  0 0.02 0])))]
                (e/client
                  (case (.setAttribute crosshair-geometry "position" (three/BufferAttribute vertices 3))
                    (binding [tb/node c]
                      (three/LineSegments crosshair-geometry crosshair-material
                        (three/DomTransparent)
                        (three/props {:position {:z -1}})))))
                new-pos ;prevent unloading
                ground-mat ;prevent unloading
                (binding [tb/camera [cube-camera c]]
                  (three/Scene
                    #_(set! (.-overrideMaterial tb/scene) (three/MeshBasicMaterial (clj->js {:color "green"})))
                    (tdom/InitCallbacksystem c)
                    (when fp
                      (three/AddObj c))
                    #_(three/AmbientLight 0x404040)

                    (three/HemisphereLight 0xffffff 0x888888 1)
                    (three/Mesh skybox skybox-mat)

                    (three/Mesh box material
                      (tdom/On "click" #(println :clicked) "")
                      (three/props {:position {:x x :y (+ y 0.5) :z z}
                                    :rotation {:y (- yaw)}}))
                    (if cb
                      (three/GridHelper 1000 1000)
                      (three/Mesh ground ground-mat
                        (three/props {:rotation {:x  tu/deg-90}})))

                    (three/Mesh box2 cube-material
                      (three/HideForCamera cube-camera)
                      (three/props {:position {:x cube-x :y 1.75 :z cube-z}})
                      (when (tdom/Hovered?)
                        (binding [tb/node cube-material]
                          (three/props {:color (three/Color 0.5 0.5 0.5)}))))

                    (let [rows 250
                          columns rows]

                      (three/InstancedMesh box phong-material (* rows columns)
                        (e/client
                          (let [node tb/node]
                            (setPosition node rows columns)))
                        (tdom/On "click" #(println (-> %
                                                     :data
                                                     (get "instanceId"))) "")

                        #_(three/props {:position {:x (* 2 (- (/ items 2) j)) :y 0.5 :z (* 2 (- (/ items 2) i))}})))))))))))))