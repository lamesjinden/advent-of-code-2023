(ns aoc.day07
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input-path "resources/day07.txt")

(def sample-input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(def card->strength {\2 2
                     \3 3
                     \4 4
                     \5 5
                     \6 6
                     \7 7
                     \8 8
                     \9 9
                     \T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14})

(def type-strength {:high-card       1
                    :one-pair        2
                    :two-pair        3
                    :three-of-a-kind 4
                    :full-house      5
                    :four-of-a-kind  6
                    :five-of-a-kind  7})

(defn hand->type [hand]
  (let [counts (->> hand
                    (frequencies)
                    (set/map-invert))
        partitions (->> hand
                        (sort)
                        (partition-by identity)
                        (group-by count))]
    (cond
      (get counts 5) :five-of-a-kind
      (get counts 4) :four-of-a-kind
      (and (get counts 3) (get counts 2)) :full-house
      (get counts 3) :three-of-a-kind
      (= 2 (count (get partitions 2 []))) :two-pair
      (= 1 (count (get partitions 2 []))) :one-pair
      :else :high-card)))

(defn parse-hand [line]
  (let [split (s/split line #"\s+")
        hand (first split)
        bid (second split)]
    {:hand (vec hand)
     :bid  (parse-long bid)}))

(defn with-hand-type [hand->type {:keys [hand] :as hand-bid}]
  (let [type (hand->type hand)]
    (-> hand-bid
        (assoc :type type)
        (assoc :type-strength (get type-strength type)))))

(defn with-hand-values [symbol->strength {:keys [hand] :as hand-bid}]
  (let [hand-values (mapv symbol->strength hand)]
    (assoc hand-bid :hand-values hand-values)))

(defn part1 []
  (->> (s/split-lines (slurp input-path))
       (map (comp (partial with-hand-values card->strength) (partial with-hand-type hand->type) parse-hand))
       (sort-by (juxt :type-strength :hand-values))
       (map-indexed (fn [index x] (* (inc index) (:bid x))))
       (apply +)))


(def card-and-joker->strength (assoc card->strength \J 1))

(defn hand-and-joker->type [hand]
  (let [joker-grouping (group-by #(= % \J) hand)
        jokers-count (count (get joker-grouping true))
        not-jokers (get joker-grouping false)
        type (hand->type not-jokers)]
    (cond
      (and (= type :high-card)
           (= jokers-count 1)) :one-pair
      (and (= type :high-card)
           (= jokers-count 2)) :three-of-a-kind
      (and (= type :high-card)
           (= jokers-count 3)) :four-of-a-kind
      (and (= type :high-card)
           (= jokers-count 4)) :five-of-a-kind

      (and (= type :one-pair)
           (= jokers-count 1)) :three-of-a-kind
      (and (= type :one-pair)
           (= jokers-count 2)) :four-of-a-kind
      (and (= type :one-pair)
           (= jokers-count 3)) :five-of-a-kind

      (and (= type :two-pair)
           (= jokers-count 1)) :full-house

      (and (= type :three-of-a-kind)
           (= jokers-count 1)) :four-of-a-kind
      (and (= type :three-of-a-kind)
           (= jokers-count 2)) :five-of-a-kind

      (and (= type :four-of-a-kind)
           (= jokers-count 1)) :five-of-a-kind

      (= jokers-count 5) :five-of-a-kind

      :else type)))

(defn part2 []
  (let [lines (s/split-lines (slurp input-path))
        hands (->> lines
                   (map (comp (partial with-hand-values card-and-joker->strength)
                              (partial with-hand-type hand-and-joker->type)
                              parse-hand))
                   (sort-by (juxt :type-strength :hand-values))
                   (map-indexed (fn [index x] (* (inc index) (:bid x))))
                   (apply +))]
    hands))


(comment

  ;; part 2
  (= :five-of-a-kind (hand-and-joker->type [\a \a \a \a \J]))
  (= :five-of-a-kind (hand-and-joker->type [\a \a \a \J \J]))
  (= :five-of-a-kind (hand-and-joker->type [\a \a \J \J \J]))
  (= :five-of-a-kind (hand-and-joker->type [\a \J \J \J \J]))
  (= :five-of-a-kind (hand-and-joker->type [\J \J \J \J \J]))
  (= :four-of-a-kind (hand-and-joker->type [\a \a \a \a \b]))
  (= :four-of-a-kind (hand-and-joker->type [\a \a \a \b \J]))
  (= :full-house (hand-and-joker->type [\a \a \b \b \J]))
  (= :three-of-a-kind (hand-and-joker->type [\a \a \b \c \J]))
  (= :three-of-a-kind (hand-and-joker->type [\a \b \c \J \J]))
  ;;; no jokers
  (= :full-house (hand-and-joker->type [\b \a \b \a \b]))
  (= :three-of-a-kind (hand-and-joker->type [\b \a \b \c \b]))
  (= :two-pair (hand-and-joker->type [\a \b \a \b \c]))
  (= :one-pair (hand-and-joker->type [\a \a \b \c \d]))
  (= :high-card (hand-and-joker->type [\a \b \c \d \e]))
  (= :five-of-a-kind (hand-and-joker->type [\a \a \a \a \a]))
  (= :four-of-a-kind (hand-and-joker->type [\b \a \a \a \a]))
  (= :full-house (hand-and-joker->type [\b \a \b \a \b]))
  (= :three-of-a-kind (hand-and-joker->type [\b \a \b \c \b]))
  (= :two-pair (hand-and-joker->type [\a \b \a \b \c]))
  (= :one-pair (hand-and-joker->type [\a \a \b \c \d]))
  (= :high-card (hand-and-joker->type [\a \b \c \d \e]))

  ;; part 1
  (= :five-of-a-kind (hand->type [\a \a \a \a \a]))
  (= :four-of-a-kind (hand->type [\b \a \a \a \a]))
  (= :full-house (hand->type [\b \a \b \a \b]))
  (= :three-of-a-kind (hand->type [\b \a \b \c \b]))
  (= :two-pair (hand->type [\a \b \a \b \c]))
  (= :one-pair (hand->type [\a \a \b \c \d]))
  (= :high-card (hand->type [\a \b \c \d \e]))

  )
