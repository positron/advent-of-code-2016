(ns advent.bathroom-code
  (:require [clojure.string :as str]))

(defn read-puzzle
  []
  (-> "bathroom-code"
      clojure.java.io/resource
      slurp
      str/trim
      (str/split #"\n")))

(defn pos->digit
  "[1 0] is the digit 2"
  [[x y]]
  (let [keypad [[1 2 3]
                [4 5 6]
                [7 8 9]]]
    (-> keypad
        (nth y)
        (nth x))))

(defn coerce
  "Applies f to n then coerces to the range [0, 3)"
  [f n]
  (-> n
      f
      (min 2)
      (max 0)))

(defmulti next-pos (fn [pos dir] dir))

(defmethod next-pos :U [[x y] dir] [x (coerce dec y)])
(defmethod next-pos :D [[x y] dir] [x (coerce inc y)])
(defmethod next-pos :R [[x y] dir] [(coerce inc x) y])
(defmethod next-pos :L [[x y] dir] [(coerce dec x) y])

(defn calculate-bathroom-num
  [directions]
  (reduce next-pos [1 1] (map (comp keyword str) directions)))

(defn calculate-bathroom-code
  [puzzle]
  (->> puzzle
      (map calculate-bathroom-num)
      (map pos->digit)
      str/join
      (println "Bathroom code: ")))

(defn solve-puzzle
  []
  (-> (read-puzzle)
      calculate-bathroom-code))
