(ns new-transcience.core
  (:use [jayq.core :only [$ css inner]])
  (:require [clojure.browser.repl :as repl]
             [new-transcience.engine :as engine]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(.log js/console "hello world")

(inner ($ :#stuff) "Changed from cljs!")

;; So we can connect to the repl server
(repl/connect "http://localhost:9001/repl")

(def ball (engine/create-circle {:color "black" }))

;; Hash-map of what should run
(def game-map-loop (atom {}))

(defn run-functions [fn-array]
  (doseq [func fn-array]
    (func)))

(defn run-game-loop []
  (run-functions (vals @game-map-loop)))

(def game-loop (js/setInterval run-game-loop 1e3))

(def ->key 
  {37 :left
   38 :down
   39 :right
   40 :up
   32 :space})

(def input (atom {}))
(def blocks (atom {}))

(defn make-block [row column]
  (if-let [old-block (@blocks [row column])]
    (do 
      (engine/destroy-shape old-block)
      (swap! blocks dissoc [row column]))
    (swap! blocks assoc [row column] @(engine/create-square {:x (* 30 column) :y (* 30 row) :h 30 :w 30}))))

(make-block 4 3)
(make-block 5 5)

(defn input? [key] (@input key))


(defn bottom-right [{:keys [x y w h r]}]
  ;;Account for circles whose x and y represent the center
  ;;instead of the top left
  (if r
    {:x (+ x r)
     :y (+ y r)}
    {:x (+ x w)
     :y (+ y h)}))

(defn top-left [{:keys [x y r]}]
  ;;Account for circles whose x and y represent the center
  ;;instead of the top left
  (if r
    {:x (- x r)
     :y (- y r)}
    {:x x
     :y y}))

(defn collision? [obj1 obj2]
  (let [br  (bottom-right obj1)
        tl  (top-left obj1)
        br2 (bottom-right obj2)
        tl2 (top-left obj2)]
    (and 
      ;;If the tops are higher than the bottoms
      (and (< (:y tl) (:y br2))
           (< (:y tl2) (:y br)))
      ;;And the lefts are "lefter" than the rights
      (and (< (:x tl) (:x br2))
           (< (:x tl2) (:x br))))))


(defn colliding? [me] 
  (let [blocks-list (vals @blocks)]
    (first (filter #(collision? me %) blocks-list))))

(comment
  (engine/create-square {:h 100 :w 100 :x 100 :y 100})
  @ball
  (colliding? @ball)


  )

(defn move [me]
  (let [speed 2
        vx (cond
             (input? :left) (- speed)
             (input? :right) speed
             :else 0)
        moved (update-in me [:x] + vx)]
    (if (zero? vx) 
      me
      (if-let [block (colliding? moved)]
        (let [block-edge (if (< vx 0)
                           (+ (:x block) (:w block) (:r me))
                           (- (:x block) (:r me)))]

          (assoc me :x block-edge))
        moved))))

(defn reset [me]
  (if (> (:y me) 650)
    (-> me
        (assoc :x 30)
        (assoc :y 30)
        (assoc :vy 0))
    me))

(defn gravity [{:keys [vy y] :as me}]
  (let [g 0.2
        vy (or vy 0)
        neue-vy (+ vy g)
        dir (if (< neue-vy 0) :up :down)
        moved (update-in me [:y] + neue-vy)]
    (if-let [block (colliding? moved)]
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
    (if (and (input? :space)
              (and (not (:jumping me)) (zero? (:vy me))))
      (assoc me :vy speed
              :jumping true)
      me)))


(defn update-player [me]
  (-> me
      (gravity)
      (move)
      (jump)
      (reset)
      ))


(set! (.-onkeydown js/document) #(swap! input assoc (->key (.-keyCode %)) true))

(set! (.-onkeyup js/document) #(swap! input assoc (->key (.-keyCode %)) false))

(def game (js/setInterval #(swap! ball update-player) 5))

(comment

  (def player
    (atom 
      {:x 0}))

  (def sq (engine/create-square {:w 100 :h 100} ))

  (swap! blocks assoc [0 0] sq)


  (swap! sq assoc :x 100 :y 100)
  @ball
  @sq


  (input? :left)

  (update-player @ball)

  (def game (js/setInterval #(swap! ball update-player) 10))
  (js/clearInterval game)


  (swap! player update-in [:motion] conj :right)


  (swap! game-map-loop assoc :test #(do (println "foo") (.log js/console "lololol")))

  (swap! game-map-loop assoc :test #(.log js/console "foo"))


  game-map-loop
  (run-game-loop)


  (disj #{4} 4)


  (vals {:foo 1 :bar 2 :zoo 3 })


  (+ 1 2)

  (in-ns 'new-transcience.core)

  (swap! ball assoc :y 50)
  (swap! ball assoc :x 80 :y 100)
  (swap! ball update-in [:x] inc)


  (inner ($ "#stuf") "asdf")

  (.unbind ($ "body"))

  (.keypress ($ "body") #(.log js/console "event: " (.-which %)))

  

  (def asdff (js/setInterval #(.log js/console (:left @input?)) 1e3))
  (js/clearInterval asdff)
  input?


  (defn move-player-fn [direction]
    (.log js/console "press" (.-which e))
    (condp = (.-which e)
      :left  #(swap! ball update-in [:x] dec)
      :down  #(swap! ball update-in [:y] dec)
      :right #(swap! ball update-in [:x] inc)
      :up    #(swap! ball update-in [:y] inc)
      #()))

  (defn keydown [direction]
    (swap! game-map-loop assoc :player-motion (move-player-fn direction)))
      

  (set! (.-title js/document) "asdfff")

  (set! (.-onkeydown js/document) check-key)


  (defn ->30th [v]
    (Math/floor (/ v 30)))

  (defn parse-canvas-click [e]
    (let [x (.-pageX e)
          y (.-pageY e)
          offset (.offset ($ "#demoCanvas"))
          left-offset (.-left offset)
          top-offset (.-top offset)
          c (->30th (- x left-offset))
          r (->30th (- y top-offset))]
      (make-block r c)))

  (set! (.-onclick js/document) parse-canvas-click)



  )



