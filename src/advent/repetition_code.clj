(ns advent.repetition-code
  (:require [clojure.string :as str]))

(defn read-signal
  []
  (-> "repetition-code"
      clojure.java.io/resource
      slurp
      str/trim
      (str/split #"\n")))

(defn denoise-signal
  [signal]
  (->> signal
       frequencies
       ;(sort-by #(- (val %))) ; puzzle 1
       (sort-by val) ; puzzle 2
       first
       key))

(defn solve-puzzle
  []
  (->> (read-signal)
       (apply map list)
       (map denoise-signal)
       (str/join)))
