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
        sector-id (Integer/parseInt sector-id)
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
       (map :sector-id)
       (reduce +)))

(defn shift-char
  "Shift char by amount, except - turns into a space"
  [amount ch]
  (if (= ch \-)
    \ 
    (let [a (int \a)
          ch (- (int ch) a)
          shifted (mod (+ ch amount) 26)
          shifted (+ shifted a)]
      (char shifted))))

(defn decrypt-room
  [room]
  (let [sector-id (:sector-id room)
        encrypted-name (:name room)
        decrypted-name (str/join (map (partial shift-char sector-id) encrypted-name))]
    (assoc room :name decrypted-name)))

(defn search-for-north-pole-room
  []
  (->> (get-valid-rooms)
       (map decrypt-room)
       (filter #(re-find #"north.*pole" (:name %)))))
