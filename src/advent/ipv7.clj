(ns advent.ipv7
  (:require [clojure.string :as str]))

(defn read-ips
  []
  (-> "ipv7"
      clojure.java.io/resource
      slurp
      str/trim
      (str/split #"\n")))

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

(defn supports-tls?
  [ip]
  (let [parsed-ip (str/split ip #"\[|]")
        ; section of the ip address not enclosed in square brackets (always first section)
        supernets (take-nth 2 parsed-ip)
        ; section of the ip address enclosed in square brackets
        hypernets (take-nth 2 (rest parsed-ip))]
    (and (some contains-abba? supernets)
         (every? #(not (contains-abba? %)) hypernets))))

(defn count-tls-ips
  []
  (->> (read-ips)
       (filter supports-tls?)
       count))
