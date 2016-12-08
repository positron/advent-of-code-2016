(ns advent.easter-hq
  (:require [clojure.string :as str]))

(defn N [[x y] distance] [x (+ y distance)])
(defn E [[x y] distance] [(+ x distance) y])
(defn S [[x y] distance] [x (- y distance)])
(defn W [[x y] distance] [(- x distance) y])

(def cardinal-directions [N E S W])

(defn apply-turn [current-direction turn]
  "Direction is represented by the int index into `cardinal-directions`"
  (let [incremement (if (= turn \R) 1 3)]
    (-> current-direction
        (+ incremement)
        (mod 4))))

(defn apply-instruction [pos direction distance]
  ((nth cardinal-directions direction) pos distance))

(defn read-instruction-stream
  []
  (-> "easter-hq"
      clojure.java.io/resource
      slurp
      str/trim
      (str/split #", ")))

(defn calculate-easter-hq-location
  [stream]
  (loop [pos [0 0] direction 0 stream stream]
    (if-let [instruction (first stream)]
      (let [turn (first instruction)
            distance (Integer/parseInt (apply str (rest instruction)))
            new-direction (apply-turn direction turn)
            new-pos (apply-instruction pos new-direction distance)]
        (recur new-pos new-direction (rest stream)))
      pos)))

(defn solve-puzzle
  []
  (->> (read-instruction-stream)
      calculate-easter-hq-location
      (map #(Math/abs %))
      (reduce +)
      (str "easter hq location distance: ")
      println))
