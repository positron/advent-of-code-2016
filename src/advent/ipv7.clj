(ns advent.ipv7
  (:require [clojure.string :as str]))

(defn read-ips
  []
  (-> "ipv7"
      clojure.java.io/resource
      slurp
      str/trim
      (str/split #"\n")))

; Puzzle 1
(defn contains-abba?
  "Returns true if the string contains a 4 letter palindrome.
  All four characters matching is not a legal abba."
  [string]
  (let [one (seq string)
        two (rest one)
        three (rest two)
        four (rest three)
        is-abba? (fn [one two three four]
                   (and (not= one two)
                        (= one four)
                        (= two three)))]
    (some identity (map is-abba? one two three four))))

(defn parse-ip
  [ip]
  (let [parsed-ip (str/split ip #"\[|]")
        ; sections of the ip address not enclosed in square brackets (always first section)
        supernets (take-nth 2 parsed-ip)
        ; sections of the ip address enclosed in square brackets
        hypernets (take-nth 2 (rest parsed-ip))]
    [supernets hypernets]))

(defn supports-tls?
  [ip]
  (let [[supernets hypernets] (parse-ip ip)]
    (and (some contains-abba? supernets)
         (every? #(not (contains-abba? %)) hypernets))))

(defn count-tls-ips
  []
  (->> (read-ips)
       (filter supports-tls?)
       count))

; Puzzle 2
(defn extract-abas
  "Extracts abas, including overlapping patterns.
  [zgrgfgg] -> [grg gfg]"
  [string]
  (let [one (seq string)
        two (rest one)
        three (rest two)]
    (->> (map (fn [c1 c2 c3]
                (when (and (= c1 c3)
                           (not= c1 c2))
                  (str c2 c1 c2)))
              one two three)
         (filter #(not (nil? %))))))

(defn contains-a-bab?
  [strings babs]
  (let [contains-bab (fn [s] (map #(str/includes? s %) babs))]
    (->> strings
         (map contains-bab)
         (map #(some identity %))
         (some identity))))

(defn supports-ssl?
  [ip]
  (let [[supernets hypernets] (parse-ip ip)
        babs (apply concat (map extract-abas supernets))]
    (contains-a-bab? hypernets babs)))

(defn count-ssl-ips
  []
  (->> (read-ips)
       (filter supports-ssl?)
       count))
