(ns advent.triangles
  (:require [clojure.string :as str]))

(defn read-triangles
  []
  (-> "triangles"
      clojure.java.io/resource
      slurp
      (str/split #"\n")))

(defn is-triangle-legal?
  [triangle]
  (let [triangle (sort > triangle)
        longest (first triangle)
        other-two (apply + (rest triangle)) ]
    (> other-two longest)))

; triangles specified horizontally, 1 per line
(def get-triangles-puzzle-1 identity)

(defn transpose
  "Returns the columns of a list of lists"
  [three-lines]
  (apply map list three-lines))

; triangles specified vertically, 3 per 3 lines
(defn get-triangles-puzzle-2
  [lines]
  (->> lines
      (partition 3)
      (map transpose)
      ; flatten 1 level (reverse the partition 3)
      (mapcat identity)))

(defn print-num-legal-triangles
  []
  (->> (read-triangles)
       (map str/trim)
       (map #(str/split % #"\s+"))
       (map #(map (fn [s] (Integer/parseInt s)) %))
       get-triangles-puzzle-2
       (filter is-triangle-legal?)
       count
       (str "Number of legal triangles: ")
       println))
