;; All the awesome things to do with enemies
(ns new-transcience.enemy
  (:require [new-transcience.core :as core]
            [new-transcience.engine :as engine]))


(def ->flip-dir 
  {:right :left
   :left :right
   nil :right})


(defn move [{:keys [dir dir-time] :as enemy}]
  (let [max-dir-time 80
        input? {dir true}]
    (if (> dir-time max-dir-time)
      (assoc enemy :dir (->flip-dir dir) :dir-time 0) 
      (-> 
        enemy
        (update-in [:dir-time] inc)
        (core/move input? 2 1 1)))))

(defn dont-fall [{:keys [dir x y vx] :as enemy} acceleration]
  (let [ 
        steps-ahead (max 2 (/ vx acceleration)) ;; How far ahead we should look to make sure the dude doesn't fall
        future-state (last (take steps-ahead (iterate (comp core/gravity move) enemy)))
        future-vy (:vy future-state)]
    ;; Check to see what the future holds for the enemies y velocity
    (if (zero? future-vy)
      ;; They aren't falling lets let them be
      enemy
      ;; They are falling, uh oh lets turn them around so they don't commit suicide
      (assoc enemy :dir (->flip-dir dir)))))

(defn dont-stand-still [{:keys [dir vx] :as enemy}]
  (let [steps-ahead 5
        future-states (map :vx (take steps-ahead (iterate move enemy)))
        zero-movement (reduce #(and %1 (zero? %2)) future-states)]
    (if zero-movement
      (assoc enemy :dir (->flip-dir dir))
      enemy)))

        

(defn standard-enemy-routine [enemy]
  (-> enemy
      (core/gravity)
      (dont-fall 1)
      (dont-stand-still)
      (move)))
      

(def enemy (engine/create-circle {:color "green" }))
(swap! enemy assoc :x 50 :y 180)

(def enemy-update-fns (atom []))

(def enemy-loop
  (js/setInterval
    #(doseq [update-enemy @enemy-update-fns]
      (update-enemy))
    20))

(swap! enemy-update-fns conj #(swap! enemy standard-enemy-routine))

(comment

  (js/clearInterval enemy-loop)


  @enemy-update-fns


  (swap! enemy standard-enemy-routine )

  (dont-stand-still @enemy)

  (map :vx (take 5 (iterate move @enemy)))

  (standard-enemy-routine @enemy)

  (move @enemy)
  (dont-fall @enemy 2)
  @enemy
  (last (take 6 (iterate (comp core/gravity move) @enemy)))
  (or 0 4)

  (let [vi (:vx @enemy)
        a -0.5
        d (Math/abs (- (:x @enemy)
                       (:x (first (remove #(= 0 (:vy %)) (iterate (comp core/gravity move) @enemy))))))]
    (println vi a d)
    (+ (Math/pow vi 2) (* 2 a d)))

  
  (+ 1 2)

  ({:left true} :left)

(doseq [x (range 5)]
  (swap! enemy standard-enemy-routine ))

(swap! enemy assoc :x 50 :y 100)
)
