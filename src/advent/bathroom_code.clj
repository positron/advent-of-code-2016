(ns advent.bathroom-code
  (:require [clojure.string :as str]))

(defn read-puzzle
  []
  (-> "bathroom-code"
      clojure.java.io/resource
      slurp
      str/trim
      (str/split #"\n")))

(def keypad
 [[nil nil "1" nil nil]
  [nil "2" "3" "4" nil]
  ["5" "6" "7" "8" "9"]
  [nil "A" "B" "C" nil]
  [nil nil "D" nil nil]])

(defn pos->digit
  "Look up digits on the keypad. Returns nil if the argument is out of bounds."
  [[x y]]
  (some-> keypad
      (nth y nil)
      (nth x nil)))

(defmulti next-pos (fn [pos dir] dir))

(defmethod next-pos :U [[x y] dir] [x (dec y)])
(defmethod next-pos :D [[x y] dir] [x (inc y)])
(defmethod next-pos :R [[x y] dir] [(inc x) y])
(defmethod next-pos :L [[x y] dir] [(dec x) y])

(defn coerce-next-pos
  [pos dir]
  (let [new-pos (next-pos pos dir)]
    (if (pos->digit new-pos)
      new-pos
      pos)))

(defn calculate-bathroom-num
  [positions directions]
  (conj positions (reduce coerce-next-pos (last positions) (map (comp keyword str) directions))))

(defn calculate-bathroom-code
  [puzzle]
  (->> puzzle
      (reduce calculate-bathroom-num [[0 2]])
      rest ; chop off the initial [0 2]
      (map pos->digit)
      str/join
      (println "Bathroom code: ")))

(defn solve-puzzle
  []
  (-> (read-puzzle)
      calculate-bathroom-code))
