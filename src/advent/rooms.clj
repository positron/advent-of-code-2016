(ns advent.rooms
  (:require [clojure.string :as str]))

(defn read-rooms
  []
  (-> "rooms"
      clojure.java.io/resource
      slurp
      (str/split #"\n")))

(defn parse-room
  [room]
  (let [room (str/split room #"[-\[\]]+")
        [sector-id checksum] (take-last 2 room)
        room-name (str/join "-" (drop-last 2 room))]
    {:name room-name
     :sector-id sector-id
     :checksum checksum}))

(defn sort-frequencies
  "Sorts by frequency, then alphabetically.
   e.g. sorts [[a 3] [s 4] [d 4]] into [[d 4] [s 4] [a 3]]"
  [freqs]
  (sort 
    (fn [[l1 f1] [l2 f2]]
      (if (= f1 f2)
        (< (int l1) (int l2))
        (> f1 f2)))
    freqs))

(defn valid-room?
  [room]
  (let [room-name (str/replace (:name room) "-" "")
        frequency-map (frequencies room-name)
        frequency-vec (into [] frequency-map)
        computed-checksum (->> frequency-vec
                               sort-frequencies
                               (take 5)
                               (map first)
                               (str/join ""))]
    (= computed-checksum (:checksum room))))

(defn get-valid-rooms
  []
  (->> (read-rooms)
       (map parse-room)
       (filter valid-room?)))

; Puzzle 1
(defn sum-of-valid-sector-ids
  []
  (->> (get-valid-rooms)
       (map (comp #(Integer/parseInt %) :sector-id))
       (reduce +)))
