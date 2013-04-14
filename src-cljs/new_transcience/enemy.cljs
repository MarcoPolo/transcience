;; All the awesome things to do with enemies
(ns new-transcience.enemy
  (:require [new-transcience.core :as core]
            [new-transcience.player :as player]
            [new-transcience.engine :as engine]))

;; We're going to implement Blackboard Architechture
(def blackboard (atom []))

(+ 1 2)

(def ->flip-dir
  {:right :left
   :left :right
   nil :right})

;; Access the atom and return the immutable hash map
(defn find-player []
  @player/player)


(defn dir-of? [dir dude1 dude2]
  (let [{:keys [x y]} dude1
        x2 (:x dude2)
        y2 (:y dude2)]
    (and (core/close-enough? y y2 50)
         (core/close-enough? x x2 80)
         (condp = dir
           :left (< x x2)
           :right (> x x2)
           false))))

(defn right-of? [& args]
  (apply dir-of? :right args))

(defn left-of? [& args]
  (apply dir-of? :left args))

;
(defn chase [{:keys [dir chasing lost-delay] :as enemy}]
  (let [player (find-player)
        lost-delay (or lost-delay 0)
        max-lost-delay 200]
    (if (:phasing player)
      (update-in enemy [:lost-delay] inc)
      (if (dir-of? dir player enemy)
        (assoc enemy :chasing true)
        (if (and chasing (> max-lost-delay lost-delay))
          (assoc enemy :dir (->flip-dir dir) :lost-delay (inc lost-delay))
          (assoc enemy :chasing false :lost-delay 0))))))

(defn move [{:keys [dir dir-time chasing acc] :as enemy}]
  (let [max-dir-time 200
        acceleration (if chasing 1 acc)
        input? {dir true}]
    (if (> dir-time max-dir-time)
      (assoc enemy :dir (->flip-dir dir) :dir-time 0)
      (->
        enemy
        (update-in [:dir-time] inc)
        (player/move input? 4 acceleration 1)))))

(defn kill [{:keys [x y] :as enemy}]
 (let [player (find-player)]
   (if (core/close-enough? y (:y player) 10)
    (if (core/close-enough? x (:x player) 10)
     (player/die player))))
  enemy)

(defn dont-fall [{:keys [dir x y vx acc] :as enemy}]
  (let [
        steps-ahead (max 2 (inc (/ vx acc))) ;; How far ahead we should look to make sure the dude doesn't fall
        future-state (last (take steps-ahead (iterate (comp player/gravity move) enemy)))
        future-vy (:vy future-state)]
    ;; Check to see what the future holds for the enemies y velocity
    (if (zero? future-vy)
      ;; They aren't falling lets let them be
      enemy
      ;; They are falling, uh oh lets turn them around so they don't commit suicide
      (assoc enemy :dir (->flip-dir dir) :dir-time 0))))

(defn find-edges [{:keys [x y] :as entity}]
  (let [acc 0.5
        left-state (assoc entity :dir :left)
        right-state (assoc entity :dir :right)]
    (map
      (fn [e]
        (:x
          (first (remove #(= 0 (:vy %)) (iterate (comp player/gravity move) e)))))
      [left-state right-state])))

(defn distance-from-edge [{:keys [x edges acc] :as entity}]
  (min (map (comp Math/abs (partial - x)) edges)))

(defn efficient-dont-fall [{:keys [acc vx dir] :as entity}]
  (let [stopping-distance (/ (Math/pow vx 2) (* 2 acc))]
    (if (core/close-enough? (distance-from-edge entity) stopping-distance 10)
      ;; They are falling, uh oh lets turn them around so they don't commit suicide
      (assoc entity :dir (->flip-dir dir) :dir-time 0)
      ;; They aren't falling lets let them be
      entity)))

(defn dont-stand-still [{:keys [dir vx] :as enemy}]
  (let [steps-ahead 5
        future-states (map :vx (take steps-ahead (iterate move enemy)))
        zero-movement (reduce #(and %1 (zero? %2)) future-states)]
    (if zero-movement
      (assoc enemy :dir (->flip-dir dir) :dir-time 0)
      enemy)))

(defn reset [{:keys [x y start-x start-y] :as enemy}]
  (if (or (> x 1000) (> y 1000))
    (assoc enemy :vy 0 :vx 0 :x start-x :y start-y :edges (find-edges enemy ))
    enemy))

(defn standard-enemy-routine [enemy]
  (-> enemy
      (reset)
      (player/gravity)
      (dont-stand-still)
      (chase)
      (move)
      (kill)
    ))

(def enemy-update-fns (atom []))
(def enemies (atom []))


(defn make-enemy [x y type]
  (let [enemy (engine/create-image-character "assets/alien.png" 1 1 17 15 16)]
    (.log js/console "making an enemy at" x y)
    (swap! enemy assoc :acc 0.5 :start-x x :start-y y)
    (swap! enemy assoc :x x :y y)
    (swap! enemies conj enemy)
    (condp = type
      :normal (swap! enemy-update-fns conj #(swap! enemy standard-enemy-routine)))))

;(make-enemy 50 350 :normal)

;(def enemy (engine/create-circle {:color "green" :acc 0.5 :start-x 50 :start-y 350}))
;(swap! enemy assoc :x 50 :y 350)
;(swap! enemy-update-fns conj #(swap! enemy standard-enemy-routine))


(def enemy-loop
  (js/setInterval
    #(doseq [update-enemy @enemy-update-fns]
      (update-enemy))
    20))







