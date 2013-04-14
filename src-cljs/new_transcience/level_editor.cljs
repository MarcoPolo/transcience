(ns new-transcience.level-editor
  (:use [jayq.core :only [$ css inner ajax bind]])
  (:require [new-transcience.core :as core]
            [new-transcience.engine :as engine]
            [new-transcience.enemy :as enemy]))

(def things (atom {}))

(def start-spot (atom nil))
(def end-spot (atom nil)) 


(defn get-item-type []
  (condp = (.attr ($ "#itemType input:checked") "value")
    "impassable" :impassable-block
    "normal"     :normal-block
    "enemy"      :enemy
    "start-spot" :start-spot
    "end-spot"   :end-spot
    :other))

(defn get-level []
  (keyword (.attr ($ "#level select :selected") "value")))

(defn remove-all-from-things [type]
  (swap! 
    things   
    #(select-keys % (for [ [k v] % :when (not= (last v) type)] k))))

(declare save-thing)

(defn make-start-spot [r c]
  (let [x (* 30 c) y (* 30 r)
        start-img (engine/create-image-character "assets/startFlag.png" 1 1 0 0 16)]
    (swap! start-img assoc :x x :y y)
    (remove-all-from-things :start-spot)
    (save-thing [x y :start-spot])
    (reset! core/start-spot {:x x :y y})
    (if @start-spot
      (engine/destroy-shape @start-spot))
    (reset! 
      start-spot
      @start-img)))

(defn make-end-spot [r c]
  (let [x (* 30 c) y (* 30 r)
        end-img (engine/create-image-character "assets/endFlag.png" 1 1 0 0 16)]
    (swap! end-img assoc :x x :y y)
    (remove-all-from-things :end-spot)
    (save-thing [x y :end-spot])
    (reset! core/end-spot {:x x :y y})
    (if @end-spot
      (engine/destroy-shape @end-spot))
    (reset! 
      end-spot
      @end-img)))

  
(defn save-level [level things]
  (ajax "/saveThings" {:type "post" :data (clj->js {:things (JSON.stringify (clj->js things))
                                                    :level level})}))

(declare clean-level)


(defn fetch-level [level callback]
  (clean-level)
  (let [call (ajax "/things" {:type "get" :data {:level level}})]
    (.done call #(callback
                   (map 
                     (fn [[a b c]] [a b (keyword c)]) 
                     (vals (js->clj (JSON.parse %))))))))

(defn save-thing [[x y type]]
  (let [c (core/->30th x)
        r (core/->30th y)]
    ;; check to see if there is a previous entry for that thing, if so delete the old one
    (if (@things [r c])
      (swap! things dissoc [r c])
      (swap! things assoc [r c] [x y type])))
  [x y type]) 
  
(defn create-thing [[x y type]]
  (let [c (core/->30th x)
        r (core/->30th y)]
    (condp = type
      :impassable-block (core/make-block r c true)
      :normal-block     (core/make-block r c false)
      :start-spot       (make-start-spot r c)
      :end-spot         (make-end-spot r c)
      :enemy            (enemy/make-enemy x y :normal)
      nil))
  [x y type])


(defn parse-level [things]
  (doseq [thing things]
     (-> thing
       (save-thing)
       (create-thing))))


(defn parse-canvas-click [e]
  (let [x (.-pageX e)
        y (.-pageY e)
        offset (.offset ($ "#demoCanvas"))
        left-offset (.-left offset)
        top-offset (.-top offset)
        x (- x left-offset)
        y (- y top-offset)
        height (.height ($ "#demoCanvas"))
        width (.width ($ "#demoCanvas"))]
    (.log js/console "clicked" x y "that is" (clj->js (get-item-type)))
    (if (and (< 0 x width) (< 0 y height))
      (-> [x y (get-item-type)]
        (save-thing)
        (create-thing)))))

(defn kill-all-enemies []
  (remove-all-from-things :enemy)
  (doseq [enemy @enemy/enemies] (engine/destroy-shape @enemy))
  (reset! enemy/enemies [])
  (reset! enemy/enemy-update-fns []))

(defn get-start-spot []
  (zipmap 
    [:x :y]
    (pop (first (filter #(= :start-spot (last %)) (vals @things))))))

(defn get-end-spot []
  (zipmap 
    [:x :y]
    (pop (first (filter #(= :end-spot (last %)) (vals @things))))))

(defn clean-level []
  ;; first get rid of all the blocks
  (doseq [block (vals @core/blocks)]
    (engine/destroy-shape block))
  (reset! core/blocks {})
  ;; Kill then enemies
  (kill-all-enemies)
  (if @start-spot
    (do
      (engine/destroy-shape @start-spot)
      (reset! start-spot nil)))
  (if @end-spot
    (do
      (engine/destroy-shape @end-spot)
      (reset! end-spot nil)))
  (reset! things {}))


(set! (.-onclick js/document) parse-canvas-click)

(.click ($ :#killAllEnemies) kill-all-enemies)

(.click ($ :#saveLevel) #(save-level (get-level) @things))

(.click ($ :#loadLevel) (fn [e]
                          (fetch-level 
                            (get-level)
                            parse-level)))
(clean-level)
;(vals @core/blocks)
                                  


