(ns advent.easter-hq
  (:require [clojure.string :as str]))

(defn N [[x y] distance] [x (+ y distance)])
(defn E [[x y] distance] [(+ x distance) y])
(defn S [[x y] distance] [x (- y distance)])
(defn W [[x y] distance] [(- x distance) y])

(def cardinal-directions [N E S W])

(defn apply-turn
  [current-direction turn]
  "Direction is represented by the int index into `cardinal-directions`"
  (let [incremement (if (= turn \R) 1 3)]
    (-> current-direction
        (+ incremement)
        (mod 4))))

(defn apply-instruction
  [pos direction distance]
  ((nth cardinal-directions direction) pos distance))

(defn read-instruction-stream
  []
  (-> "easter-hq"
      clojure.java.io/resource
      slurp
      str/trim
      (str/split #", ")))

(defn print-distance
  [pos]
  (->> pos
    (map #(Math/abs %))
    (reduce +)
    (str "Easter HQ location distance: ")
    println))

(defn apply-instruction-steps
  "Travel one block at a time, checking at each block if we've been here before.
  
  Returns [locations new-position hq-location]. hq-location is nil if it hasn't
  been reached."
  [locations pos dir distance]
  (if (zero? distance)
    [locations pos nil]
    (let [new-pos (apply-instruction pos dir 1)]
      (if (contains? locations new-pos)
        [nil nil new-pos]
        (recur (conj locations new-pos) new-pos dir (- distance 1))))))

; Puzzle 1
(defn calculate-end-location
  [stream]
  (loop [pos [0 0] direction 0 stream stream]
    (if-let [instruction (first stream)]
      (let [turn (first instruction)
            distance (Integer/parseInt (apply str (rest instruction)))
            new-direction (apply-turn direction turn)
            new-pos (apply-instruction pos new-direction distance)]
        (recur new-pos new-direction (rest stream)))
      pos)))

(defn calculate-end-distance
  []
  (->> (read-instruction-stream)
      calculate-end-location
      print-distance))

; Puzzle 2
(defn calculate-easter-hq-location
  [stream]
  (loop [pos [0 0] direction 0 locations #{} stream stream]
    (if-let [instruction (first stream)]
      (let [turn (first instruction)
            distance (Integer/parseInt (apply str (rest instruction)))
            new-direction (apply-turn direction turn)
            [locations new-pos hq-loc] (apply-instruction-steps locations pos new-direction distance)]
        (if hq-loc
          hq-loc
          (recur new-pos new-direction locations (rest stream))))
      (throw (RuntimeException. "End of instructions reached without reaching HQ!")))))

(defn calculate-easter-hq-distance
  []
  (->> (read-instruction-stream)
      calculate-easter-hq-location
      print-distance))
