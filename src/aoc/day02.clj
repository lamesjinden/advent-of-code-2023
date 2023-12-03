(ns aoc.day02
  (:require [clojure.string :as s]
            [instaparse.core :as insta]
            [aoc.util :as u]))

(def input-path "resources/day02.txt")

(def game-parser
  (insta/parser
    "<game> = <'Game '> #'\\d+' <': '> subsets
     <subsets> = subset (<'; '> subset)*
     subset = subset-token (<', '> subset-token)*
     subset-token = count <' '> color
     <count> = #'\\d+'
     <color> = 'blue' | 'green' | 'red'"))

(defn line->game [s]
  (game-parser s))

(defn subset-token->map [[_ count-str color]]
  {:count (u/->int count-str)
   :color (keyword color)})

(defn subset->map [[_ & subset-tokens]]
  (->> subset-tokens
       (map subset-token->map)
       (map (fn [{:keys [color count]}] [color count]))
       (into {})))

(defn game->max-counts [[game & subsets]]
  (->> subsets
       (map subset->map)
       (apply merge-with max {:game  (u/->int game)
                              :red   0
                              :green 0
                              :blue  0})))

(defn possible? [max-counts]
  (let [discriminators {:red 12 :green 13 :blue 14}]
    (every? (fn [[k v]] (<= (get max-counts k 0) v)) discriminators)))

(defn part1 []
  (->> (-> input-path (slurp) (s/split-lines))
       (map (comp game->max-counts line->game))
       (filter possible?)
       (map :game)
       (reduce +)))

(defn set-power [max-counts]
  (reduce * (-> max-counts
                (dissoc :game)
                (vals))))

(defn part2 []
  (->> (-> input-path (slurp) (s/split-lines))
       (map (comp set-power game->max-counts line->game))
       (reduce +)))
