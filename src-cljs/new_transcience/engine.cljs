(ns new-transcience.engine
  (:use [jayq.core :only [$ append]]))

(def stage
  (createjs/Stage. "demoCanvas")) ;;new stage(demoCanvas)

;; play the shot sound
;;createjs.Sound.play("laser", createjs.Sound.INTERUPT_LATE);

(def assetsPath "assets/")

(def manifest [
               { :id "alienTalk" :src (str assetsPath "AlienTalk.wav") } ;; :data 6
               { :id "death" :src (str assetsPath "Death.wav") }
               { :id "phase" :src (str assetsPath "Phase.wav") }
               { :id "switch" :src (str assetsPath "Switch.wav") }
               { :id "tractorBeam" :src (str assetsPath "TractorBeam.wav") }
               { :id "shipAmbiance" :src (str assetsPath "ShipAmbiance.wav") }               
              ])

(def preload (createjs/LoadQueue.))
(.installPlugin preload (.-Sound js/createjs)) ;; .- for accessing object fields
(.loadManifest preload (clj->js manifest))

(def sound (atom {}))

(def sound createjs/Sound)

;(.addEventListener preload "complete" #(.play createjs/Sound "shipAmbiance"))


(defn death-sound [] (.play createjs/Sound "death"))

(defn sound-finished? [sound-name]
  (@sound sound-name))

(defn finished-sound [sound-name]
  (swap! sound assoc sound-name true))

(defn play-sound-once [sound-name] 
  (if (sound-finished? sound-name)
    (let [sound-instance (.play sound sound-name)]
      (.addEventListener sound-instance "complete" (finished-sound sound-name)))
    nil))

(defn phase-sound [] (.play createjs/Sound "phase"))

(defn switch-sound [] (.play createjs/Sound "switch"))

(defn tractor-beam-sound [] (.play createjs/Sound "tractorBeam"))

(defn ship-ambiance-sound [] (.play createjs/Sound "shipAmbiance"))
;(.play createjs/Sound  "alienTalk")











;;(.addChild stage circle)

;; (.update stage)


(defn update-screen-60hz []
  (js/setInterval #(.update stage) 15))

;;(js/clearInterval screen-refresh)
(def screen-refresh (update-screen-60hz))


(defn approach-point [x target]
  (cond 
    (< x target) (inc x)
    (> x target) (dec x)
    :else x))

;; registers a shape to update when it's atom changes
(defn register-shape-atom [shape]
  ;; Add a watch on this atom to update the screen
  (add-watch shape :movements
             (fn [k r old-state new-state]
               (let [shape (:easel-shape new-state)]
                 (set! (.-x shape) (:x new-state))
                 (set! (.-y shape) (:y new-state)))))
  shape)

(defn add-and-update-stage [shape] 
  (.addChild stage shape)
  (.update stage))

;; Returns an atom containing the state of that square
(defn create-square [{:keys [x y w h color] :or {x 0 y 0 w 10 h 10 color "blue"} :as attrs}]
  (let [square (createjs/Shape.)]
    (-> (.-graphics square)
      (.beginFill color)
      (.drawRect x y w h))
    (add-and-update-stage square)
    (register-shape-atom
      (atom (merge
              attrs
              {:easel-shape square
               :x x
               :h h
               :w w
               :y y})))))

(defn create-circle [{:keys [x y r color] :or {x 0 y 0 r 10 color "red"} :as attrs}]
  (let [circle (createjs/Shape.)]
    (-> 
        (.-graphics circle)
        (.beginFill color)
        (.drawCircle x y r))
    (add-and-update-stage circle)
    (register-shape-atom 
      (atom (merge
              attrs
              {:easel-shape circle
               :x x
               :r r
               :y y})))))

(defn destroy-shape [shape]
  (.removeChild stage (:easel-shape shape))
  (.update stage))


(defn move-item-to [entity x y speed]
  (js/clearTimeout (:movement @entity 0))
  (when (or 
          (not= x (:x @entity)) 
          (not= y (:y @entity))) 
    (swap! entity #(assoc % :x (approach-point (:x @entity) x)
                            :y (approach-point (:y @entity) y)))
    (swap! entity 
           (fn [e]
             (assoc e
                   :movement
                   (js/setTimeout #(move-item-to entity x y speed) (/ 1000 speed)))))))


(.log js/console "now we are reqady for some real development!")
