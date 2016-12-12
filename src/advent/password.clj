(ns advent.password
  (:require [clojure.string :as str]
            [digest]))

; Puzzle 1
(defn calculate-simple-password
  [id]
  (loop [pw "" nums (range)]
    (let [hash (digest/md5 (str id (first nums)))]
      ;(when (zero? (mod (first nums) 1000000)) (println (first nums))) ; print progress
      (if (= [\0 \0 \0 \0 \0] (subvec (vec hash) 0 5))
        (if (>= (count pw) 7)
          (println "Password is: " (str pw (nth hash 5)))
          (do (println (nth hash 5)) (recur (str pw (nth hash 5)) (rest nums))))
        (recur pw (rest nums))))))

; Puzzle 2
(defn print-password
  [password]
  (->> password
       (map #(if (nil? %) "_" %))
       str/join
       println))

(defn contains-nil?
  [pw]
  (not (zero? (count (filter #(= nil %) pw)))))

(defn update-pw
  "Updates password with ch at pos if there isn't already a character there"
  [pw pos ch]
  (if (and (< pos (count pw))
           (nil? (nth pw pos)))
      (assoc pw pos ch)
      pw))

(defn calculate-awesome-password
  [id]
  (loop [pw (vec (repeat 8 nil)) nums (range)]
    (let [hash (digest/md5 (str id (first nums)))]
      ;(when (zero? (mod (first nums) 1000000)) (println (first nums))) ; print progress
      (if (= [\0 \0 \0 \0 \0] (subvec (vec hash) 0 5))
        (let [pos (Integer/parseInt (str (nth hash 5)) 16)
              ch (nth hash 6)
              pw (update-pw pw pos ch)]
          (if (contains-nil? pw)
            (do (print-password pw) (recur pw (rest nums)))
            (print-password pw)))
        (recur pw (rest nums))))))
          
