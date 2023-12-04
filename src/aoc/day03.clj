(ns aoc.day03
  (:require [aoc.util :as u]))

(def input-path "resources/day03.txt")

(defn project-grid-elements
  "projection function for converting input characters to data."
  [value coords]
  (cond
    (Character/isDigit value) {:value (str value)
                               :type :value
                               :coords coords}
    (not= \. value) {:value (str value)
                     :type :symbol
                     :coords coords}
    :else nil))

(defn read-entities
  "given a grid of projected elements, read-entities partitions elements
   within a row by the element type; additionally, removes runs of nil elements.
   the resulting value is a list of lists - specifically a list of partitions."
  [grid]
  (->> grid
       (mapcat (fn [row] (partition-by :type row)))
       (filter #(not (nil? (first %))))))

(defn value-entities? [[{type :type :as _entity}]]
  (= type :value))


(defn has-neighboring-symbol?
  "returns 'true' if any coordinate pairs described by 'coords' or elements
   of 'more' have an adjacent symbol element."
  [grid coords & more]
  (->> (concat [coords] more)
       (mapcat (fn [coords] (u/neighbor-coords coords :include-diagonal true)))
       (some (fn [[row col]]
               (= (get-in grid [row col :type] grid)
                  :symbol)))))

(defn value-entities->value
  "derives the numerical value represented by multiple value entities"
  [entities]
  (->> entities
       (map :value)
       (reduce str)
       (u/->int)))

(defn part1 []
  (let [grid (-> (slurp input-path) (u/->grid project-grid-elements))]
    (->> grid
         (read-entities)
         (filter value-entities?)
         (filter (fn [xs] (apply has-neighboring-symbol? grid (map :coords xs))))
         (map value-entities->value)
         (reduce +))))

(defn gear-symbol? [{:keys [type value] :as _entity}]
  (and (= type :symbol)
       (= value "*")))

(defn grid-entities->value-lookup
  "builds a map from coordinate pairs to the associated value (as int);
   ex: given the number 15 starting at [0 0], where [0 0] contains '1' and [0 1] contains '5', 
       then the resulting map would have key [0 0] => 15 and key [0 1] => 15."
  [grid-entities]
  (->> grid-entities
       (filter value-entities?)
       (mapcat (fn [xs]
                 (let [value (value-entities->value xs)]
                   (map (fn [{coords :coords}]
                          [coords value])
                        xs))))
       (into {})))

(defn symbol-entity->part-number-neighbors [value-lookup {coords :coords}]
  (->> (u/neighbor-coords coords :include-diagonal true)
       (keep (fn [[row col]]
               (get value-lookup [row col])))
       ;; into a set to remove duplicates from having multiple
       ;; neighbors of the same value entity
       (into #{})))

(defn part2 []
  (let [grid-entities (-> (slurp input-path) (u/->grid project-grid-elements) (read-entities))
        value-entity-lookup (grid-entities->value-lookup grid-entities)]
    (->> grid-entities
         ;; begin filtering for 'gears' entities
         (filter gear-symbol?)
         (map (partial symbol-entity->part-number-neighbors value-entity-lookup))
         (filter #(= (count %) 2))
         ;;end filtering for 'gears
         (map (partial reduce *))
         (reduce +))))
