(ns advent.password
  (:require [clojure.string :as str]
            [digest]))

(defn calculate-password
  [id]
  (loop [pw "" nums (range)]
    (let [hash (digest/md5 (str id (first nums)))]
      ;(when (zero? (mod (first nums) 1000000)) (println (first nums))) ; print progress
      (if (= [\0 \0 \0 \0 \0] (subvec (vec hash) 0 5))
        (if (>= (count pw) 7)
          (println "Password is: " (str pw (nth hash 5)))
          (do (println (nth hash 5)) (recur (str pw (nth hash 5)) (rest nums))))
        (recur pw (rest nums))))))
