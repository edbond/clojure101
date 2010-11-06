(ns clojure101.game_of_life
  (:gen-class)
  (:require [clojure.contrib.swing-utils :as swing]
            [clojure.java.javadoc :as javadoc])
  (:use [clojure.contrib.pprint :only [pprint]])
  (:import
   (java.awt Color Graphics Dimension)
   (java.awt.image BufferedImage)
   (javax.swing JPanel JFrame)))

; (set! *warn-on-reflection* true)

(def field (ref {}))

(defn pp-field
  [field padding]
  (let [ones field ;(into {} (filter #(not (zero? (last %))) field))
        xx (->> (keys ones) (map first))
        yy (->> (keys ones) (map last))
        minx (- (apply min xx) padding)
        maxx (+ (apply max xx) padding)
        miny (- (apply min yy) padding)
        maxy (+ (apply max yy) padding)
        print-row (fn [y]
                    (println 
                     (map #(get field [% y] 0) (range (inc minx) maxx))))]
    (dorun (map #(print-row %) (range (inc miny) maxy)))))

(defn make-field
  [ones]
  (zipmap ones (repeat 1)))

(def blinker
     (make-field [[1 2] [2 2] [3 2]]))

(def r-pentomino
     (make-field [[1 2][2 1][2 2][2 3][3 1]]))

(defn next-state
  "Returns next state for cell based on number of neighbors"
  [state neighbors-count]
  (cond
   ; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
   (and (= state 0) (= neighbors-count 3)) 1
   ; Any live cell with two or three live neighbours lives on to the next generation.
   (and (= state 1) (<= 2 neighbors-count 3)) 1
   ; Any live cell with more than three live neighbours dies, as if by overcrowding.
   ; Any live cell with fewer than two live neighbours dies, as if caused by under-population.
   :else 0))

;; Neighbors functions
(defn neighbors
  "Return neighbors indexes"
  [field x y]
  (let [indexes [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        xy (map (fn [[a b]] [(+ x a) (+ y b)]) indexes)]
    xy))

(defn neighbors-vals
  "Return values for field in neighbor cells"
  [field x y]
  (let [neighbors (neighbors field x y)]
    (map #(get field % 0) neighbors)))

(defn neighbors-sum
  [field x y]
  (reduce + (neighbors-vals field x y)))

(defn cell-next-state
  [field x y]
  (let [cell-val (get field [x y] 0)
        neighbors-sum (neighbors-sum field x y)]
    (next-state cell-val neighbors-sum)))

(defn next-population
  "For each cell and it's neighbors get next state"
  [field]
  (let [cells (keys field)
        cells-and-neighbors (mapcat (fn [[x y]] (neighbors field x y)) cells)
        next-state (fn [acc [x y]]
                     (let [next-state (cell-next-state field x y)]
                       (if (= 1 next-state)
                         (assoc acc [x y] next-state)
                         acc)))]
    (reduce next-state {} (distinct cells-and-neighbors))))

(defn nth-population
  [field n]
  (if (zero? n)
    field
    (recur (next-population field) (dec n))))

;; GUI
(def dim 40)

(defn render-field
  [g]
  (for [x (range 0 dim) y (range 0 dim) :when ([x y] @field)]
    (doto g
      (.setColor (Color/blue))
      (.fillRect x y 1 1))))

(defn render
  [g]
  (let [v blinker
        img (BufferedImage. dim dim
                 (. BufferedImage TYPE_INT_ARGB))
        bg (.getGraphics img)]
    (doto bg
      (.setColor (Color/white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun
     (render-field bg))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (render g)))
             (.setPreferredSize (new Dimension 
                                     dim
                                     dim))))

(def animation-sleep-ms 100)
(def running (ref false))
(def animator (agent nil))

(defn animation [x]
  (when @running
    (send-off *agent* #'animation))
  (.repaint panel)
  (.sleep Thread animation-sleep-ms)
  nil)

;; (send-off animator animation)
