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

(defn print-num-legal-triangles
  []
  (->> (read-triangles)
       (map str/trim)
       (map #(str/split % #"\s+"))
       (map #(map (fn [s] (Integer/parseInt s)) %))
       (filter is-triangle-legal?)
       count
       (str "Number of legal triangles: ")
       println))
