(ns new-transcience.level-editor 
  (:use [jayq.core :only [$ css inner ajax bind]])
  (:require [new-transcience.core :as core]))


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
    :other))

  

(defn parse-canvas-click [e]
  (let [x (.-pageX e)
        y (.-pageY e)
        offset (.offset ($ "#demoCanvas"))
        left-offset (.-left offset)
        top-offset (.-top offset)
        c (core/->30th (- x left-offset))
        r (core/->30th (- y top-offset))]
    (.log js/console "clicked" r c "that is" (get-item-type))
    (condp = (get-item-type)
      :impassable-block (core/make-block r c true)
      :normal-block     (core/make-block r c false)
      :enemy            #(.log js/console "Should be making an enemy")
      nil)))


(set! (.-onclick js/document) parse-canvas-click)

