(ns aoc.day04
  (:require [clojure.math :as math]
            [clojure.string :as s]
            [clojure.set :as set]
            [instaparse.core :as insta]
            [aoc.util :as u])
  (:import [java.util LinkedList]))

(def input-path "resources/day04.txt")

(def sample-input
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card  6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def card-parser
  (insta/parser
   "card = <'Card'> <#'\\s'>+ #'\\d+' <':'> <#'\\s'>+  winners <#'\\s'>+ <'|'> <#'\\s'>+ numbers
    winners = #'\\d+' (<#'\\s'>+ #'\\d+')*
    numbers = #'\\d+' (<#'\\s'>+ #'\\d+')*"))

(defn parse-card [s]
  (card-parser s))

(defn card->matches [[_ _ [_ & winners] [_ & numbers] :as _card]]
  (let [winners (map u/->int winners)
        numbers (map u/->int numbers)]
    (set/intersection (into #{} winners) (into #{} numbers))))

(defn evaluate-card [card]
  (let [matches (card->matches card)]
    (if (seq matches)
      (math/pow 2 (dec (count matches)))
      0)))

(defn part1 []
  (->> (slurp input-path)
       (s/split-lines)
       (map (comp evaluate-card parse-card))
       (apply +)))

(defn cards->lookup
  "creates a map of cards, associating card id to the card map"
  [cards]
  (->> cards
       (map (fn [{id :id :as card}]
              [id card]))
       (into {})))

(defn card->map
  "converts parsed card data to a map;
   the map contains information to support processing, 
   and, most importantly, is computed up-front - and not in the loop/recur"
  [[_ card-number [_ & winners] [_ & numbers]]]
  ;; keep the id, converted to int; it will be used as a key into the card lookup
  {:id (u/->int card-number)
   ;; precompute the matching set count
   :matches (count (set/intersection (->> winners (map u/->int) (into #{}))
                                     (->> numbers (map u/->int) (into #{}))))})

(defn process-cards-maps
  "processes card-maps;
   uses several imperative, mutable optimizations to improve runtime:
   * uses java.util.LinkedList as (mutable) queue
   * computes the resulting count value per iteration (via `atom`)
   * takes advantage of updated data model
     * precomputed intersection count
     * avoids sequential destructuring within the loop/recur"
  [cards]
  (let [card-lookup (cards->lookup cards)
        processed (atom 0)]
    (loop [cards (LinkedList. (map :id cards))]
      (if-let [card-id (.poll cards)]
        (let [card (get card-lookup card-id)
              matches (:matches card)
              copy-ids (->> (iterate inc (inc card-id))
                            (take matches)
                            (filter #(get card-lookup %)))]
          (swap! processed inc)
          (recur
           (doto cards
             (.addAll copy-ids))))
        @processed))))

(defn part2
  "measured runtime: 10.5s"
  []
  (->> (slurp input-path)
       (s/split-lines)
       (map (comp card->map parse-card))
       (process-cards-maps)))