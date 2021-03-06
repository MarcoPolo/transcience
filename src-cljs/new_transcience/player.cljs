(ns new-transcience.player
  (:use [jayq.core :only [$ css inner ajax bind]])
  (:require [new-transcience.core :as core]
            [new-transcience.engine :as engine]))

(declare player)


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
  (let [{:keys [x y]} (or @core/start-spot {:x 50 :y 0} )]
    ;;reset if the player is out of bounds or finished with the game
    (if (or (> (:y me) 650) (:finished me) (:killed me))
      (-> me
          (assoc :x x)
          (assoc :y y)
          (assoc :vy 0)
          (assoc :vx 0)
          (assoc :finished false)
          (assoc :killed false))
      me)))

(defn check-finish [{:keys [x y] :as me}]
  (let [end-spot (or @core/end-spot {:x 550 :y 400} )]
    (if (and (core/close-enough? x (:x end-spot) 30)
             (core/close-enough? y (:y end-spot) 30))
      (do
        (core/next-level)
        (assoc me :finished true))
      me)))


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


(defn phase [{:keys [x y starting-phasing-spot] :as me}]
  (let [max-phasing-cycles 20
        cool-down-cycles 20]
    (if (:phasing me)
      (if (> max-phasing-cycles (:phasing-count me))
        (update-in me [:phasing-count] inc)
        (if (core/colliding? (assoc me :phasing false))
          ;(assoc me :x (first starting-phasing-spot) :y (second starting-phasing-spot) :phasing false :phasing-count 0 :cool-down-count cool-down-cycles)
          (core/first-non-colliding starting-phasing-spot (assoc me :phasing false :phasing-count 0 :cool-down-count 0))
          (assoc me :phasing false :phasing-count 0 :cool-down-count 0)))
      (if (> cool-down-cycles (:cool-down-count me))
        (update-in me [:cool-down-count] inc)
        (if (core/input? :phase)
          (assoc me :starting-phasing-spot [x y] :phasing true :cool-down-count 0 :phasing-count 0)
          me)))))



(defn change-color [me]
  (let [new-color "red"
        old-color "black"
        easel (:easel-shape me)]
    (if (and (:phasing me) (not (:painted me)))
      (do
        (set! (.-image easel) (first ($ :#char-trans)))
        (assoc me :painted true))
      (if (and (not (:phasing me)) (:painted me))
        (do
          (set! (.-image easel) (first ($ :#char-normal)))
          (assoc me :painted false))
        me))))

(defn die [me]
  (swap! player assoc :killed true))

(defn update-player [me]
  (-> me
      (gravity)
      (move core/input? 15 0.5 1)
      (jump)
      (phase)
      (change-color)
      (reset)
      (check-finish)
      ))

(def player (engine/create-image-character "assets/main-character.png" 0.5 0.5 25 30 10))

;; Start the player loop
(def game (js/setInterval #(swap! player update-player) 15))

(add-watch core/start-spot :reset
           (fn [k r old-state new-state]
             (die @player)))

(comment 

  (first (for [x (range 10) y (range 10) :when (every? even? [x y])] [x y]))


  )
