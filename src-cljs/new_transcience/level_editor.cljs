(ns new-transcience.level-editor
  (:use [jayq.core :only [$ css inner ajax bind]])
  (:require [new-transcience.core :as core]
            [new-transcience.enemy :as enemy]))

(defn build-demo-level []
  (let [call (ajax "/blocks" {:type "get" })]
    (.done call #(let [server-blocks (vals (cljs.reader/read-string %))]
                   (doseq [old-block (keys @core/blocks)]
                     (apply core/make-block old-block))
                   (doseq [new-block server-blocks]
                     (apply core/make-block new-block))))))

(build-demo-level)

(defn get-item-type []
  (condp = (.attr ($ "#itemType input:checked") "value")
    "impassable" :impassable-block
    "normal"     :normal-block
    "enemy"      :enemy
    :other))



(defn parse-canvas-click [e]
  (let [x (.-pageX e)
        y (.-pageY e)
        offset (.offset ($ "#demoCanvas"))
        left-offset (.-left offset)
        top-offset (.-top offset)
        x (- x left-offset)
        y (- y top-offset)
        height (.height ($ "#demoCanvas"))
        width (.width ($ "#demoCanvas"))
        c (core/->30th x)
        r (core/->30th y)]
    (.log js/console "clicked" r c "that is" (get-item-type))
    (if-not (or (> x width) (> y height))
      (condp = (get-item-type)
        :impassable-block (core/make-block r c true)
        :normal-block     (core/make-block r c false)
        :enemy            (enemy/make-enemy x y)
        nil))))


(set! (.-onclick js/document) parse-canvas-click)

