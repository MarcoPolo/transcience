(ns new-transcience.engine
  (:use [jayq.core :only [$ append]]))

(def stage
  (createjs/Stage. "demoCanvas"))

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

(defn create-impassable-block [{:keys [x y w h] :or {x 0 y 0 w 30 h 30} :as attrs}]
  (let [impassable-block (createjs/Bitmap. "assets/impassable-block.png")]
    (set! (.-x impassable-block) x)
    (set! (.-y impassable-block) y)
    (add-and-update-stage impassable-block)
    (atom (merge attrs {:easel-shape impassable-block}))))

  

(defn create-image-character [assetUrl scaleX scaleY regX regY rad]
  (let [player (createjs/Bitmap. assetUrl)]
    (set! (.-scaleX player) scaleX)
    (set! (.-scaleY player) scaleY)
    (set! (.-regX player) regX)
    (set! (.-regY player) regY)
    (add-and-update-stage player)
    (register-shape-atom
      (atom {:x 0 :y 0 :r rad :easel-shape player}))))

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
