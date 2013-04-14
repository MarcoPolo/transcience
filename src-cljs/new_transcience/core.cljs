(ns new-transcience.core
  (:use [jayq.core :only [$ css inner ajax bind]])
  (:use-macros [cljs.core :only [this-as]])
  (:require [clojure.browser.repl :as repl]
            [new-transcience.engine :as engine]))

(.log js/console "hello world")

;; So we can connect to the repl server
(repl/connect "http://localhost:9000/repl")

(def ->key
  {37 :left
   38 :down
   39 :right
   40 :up
   16 :phase
   32 :space})

(def input (atom {}))

(def blocks (atom {}))

(defn make-block [row column impassable]
  (if-let [old-block (@blocks [row column])]
    (do
      (engine/destroy-shape old-block)
      (swap! blocks dissoc [row column]))
    (swap! blocks assoc [row column]
           (assoc
             (if impassable
               @(engine/create-impassable-block {:x (* 30 column) :y (* 30 row) :h 30 :w 30})
               @(engine/create-square {:x (* 30 column) :y (* 30 row) :h 30 :w 30 :color "#2c677d"}))
             :impassable
             impassable))))

core/start-spot

(defn ->30th [v]
  (Math/floor (/ v 30)))

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
    (if (:phasing me)
      (first (filter #(and (:impassable %) (collision? me %)) blocks-list))
      (first (filter #(collision? me %) blocks-list)))))

(defn close-enough? [num1 num2 max-dist]
  (< (Math/abs (- num1 num2)) max-dist))

(defn onkeypress [e]
  (swap! input assoc (->key (.-keyCode e)) true)
  (if (->key (.-keyCode e))
    (.preventDefault e)))


(def start-spot (atom nil))
(def end-spot (atom nil))
(def current-level (atom 1))

(defn next-level []
  (swap! current-level inc)
  (new-transcience.level-editor/fetch-level @current-level new-transcience.level-editor/parse-level)
  )

(set! (.-onkeydown js/document) onkeypress)

(set! (.-onkeyup js/document) #(swap! input assoc (->key (.-keyCode %)) false))
