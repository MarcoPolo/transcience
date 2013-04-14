(ns new-transcience.player
  (:require [new-transcience.core :as core]
            [new-transcience.engine :as engine]))

(defn move [{:keys [vx x] :as me} input? max-speed acceleration decelartion]
  (let [decelartion (if (:jumping me) 0.1 decelartion)
        vx (or vx 0)
        neue-vx (cond
                  (:phasing me) vx
                  (input? :left) (max (- max-speed) (- vx acceleration))
                  (input? :right) (min max-speed (+ vx acceleration))
                  :else (if (< -1 vx 1) 0 (if (pos? vx) (- vx decelartion) (+ vx decelartion))))
        moved (update-in me [:x] + neue-vx)]
    (if-let [block (core/colliding? moved)]
      (let [block-edge (if (< neue-vx 0)
                         (+ (:x block) (:w block) (:r me))
                         (- (:x block) (:r me)))]
        (assoc me :x block-edge
                  :vx 0))
      (-> moved
          (assoc :vx neue-vx)))))

(defn reset [me]
  (if (> (:y me) 650)
    (-> me
        (assoc :x 50)
        (assoc :y 50)
        (assoc :vy 0))
    me))

(defn gravity [{:keys [vy y] :as me}]
  (let [g 0.8
        vy (or vy 0)
        neue-vy (if (:phasing me) vy (+ vy g))
        dir (if (< neue-vy 0) :up :down)
        moved (update-in me [:y] + neue-vy)]
    (if-let [block (core/colliding? moved)]
      (let [block-edge (if (= dir :up)
                         (+ (:y block) (:h block) (:r me))
                         (- (:y block) (:r me)))]
        (assoc me :y block-edge
               :jumping (= dir :up)
               :vy 0))
      (-> moved
        (assoc :vy neue-vy)))))

(defn jump [me]
  (let [speed -10]
    (if (and (core/input? :space)
             (and (not (:jumping me)) (zero? (:vy me)))
             (not (:phasing me)))
      (assoc me :vy speed
              :jumping true)
      me)))


(defn phase [me]
  (let [max-phasing-cycles 20
        cool-down-cycles 20]
    (if (:phasing me)
      (if (> max-phasing-cycles (:phasing-count me))
        (update-in me [:phasing-count] inc)
        (assoc me :phasing false :phasing-count 0 :cool-down-count 0))
      (if (> cool-down-cycles (:cool-down-count me))
        (update-in me [:cool-down-count] inc)
        (if (core/input? :phase)
          (assoc me :phasing true :cool-down-count 0 :phasing-count 0)
          me)))))


(defn change-color [me]
  (let [new-color "red"
        old-color "black"]
    (if (and (:phasing me) (not (:painted me)))
      (do
        (-> (:easel-shape me)
          (.-graphics)
          (.clear)
          (.beginFill new-color)
          (.drawCircle 0 0 (:r me))
          )
        (assoc me :painted true))
      (if (and (not (:phasing me)) (:painted me))
        (do
          (-> (:easel-shape me)
            (.-graphics)
            (.clear)
            (.beginFill old-color)
            (.drawCircle 0 0 (:r me)))
          (assoc me :painted false))
        me))))


(defn update-player [me]
  (-> me
      (gravity)
      (move core/input? 15 0.5 1)
      (jump)
      (phase)
      (reset)
      ))

(def player (engine/create-image-character "assets/main-character.png" 0.5 0.5 25 25 12.5))

;; Start the player loop
(def game (js/setInterval #(swap! player update-player) 15))
