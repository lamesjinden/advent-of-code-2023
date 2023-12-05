(ns aoc.util
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m])
  (:import (java.util.regex Pattern)))

(defn ->int
  "parses s to Integer"
  [s]
  (Integer/parseInt s))

(defn ->double
  "parses s to Double"
  [s]
  (Double/parseDouble s))

(defn flip-lr
  "returns the result of flipping m horizontally"
  [m] (m/matrix (m/slice-map reverse m)))

(defn flip-ud
  "returns the result of flipping m vertically"
  [m] (m/matrix (reverse (m/rows m))))

(defn rotate-cw
  "returns the result of rotating m 90 degrees clockwise"
  [m] (m/matrix (m/slice-map reverse (m/transpose m))))

(defn rotate-ccw
  "returns the result of rotating m 90 degrees counter-clockwise"
  [m] (m/matrix (reverse (m/transpose m))))

(defn vstack
  "returns the result of joining ms vertically"
  [& ms] (apply m/join-along 0 ms))

(defn hstack
  "returns the result of joining ms horizontally"
  [& ms] (apply m/join-along 1 ms))

(defn split-on-newline
  "returns a lazy sequence of sequences of strings that is the result of
  splitting s on newline characters, partitioning on blank lines,
  then excluding blank elements"
  [s]
  (->> s
       (str/split-lines)
       (partition-by str/blank?)
       (filter #(not (every? str/blank? %)))))

(defn index-by
  "accepts a number of map instances and produces a single map
  that associates the result of applying f to each map to the
  respective map"
  [keyfn & maps]
  (->> maps
       (map (fn [m]
              [(keyfn m) m]))
       (into {})))

(defn indexcat-by
  "similar to index-by, but where keyfn produces a collection.
  each element of the result of applying keyfn to each map is associated
  to the respective map.
  the resulting maps are merged via merge-with using the provided
  mergefn"
  [keyfn mergefn & maps]
  (->> maps
       (mapcat (fn [m] (map (fn [n] {n m}) (keyfn m))))
       (apply merge-with mergefn)))

(defn coords-2d
  "returns a lazy sequence of all possible x,y pairs representing
  locations within grid. pairs are represented as vectors of 2 elements
  (e.g. [<ROW> <COLUMN>])"
  [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [row (range rows)
          col (range cols)]
      [row col])))

(defn coords-2d-columnar
  "returns a lazy sequence of all possible x,y pairs representing
  locations within grid. pairs are represented as vectors of 2 elements
  (e.g. [<ROW> <COLUMN>])"
  [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [col (range cols)
          row (range rows)]
      [row col])))

(defn count-by
  "returns a map of the counts of the elements of coll keyed by the result
  of f on each element. similar to group-by where values are counts of
  items grouped by f. also, see frequencies"
  [f coll]
  (let [grouped (group-by f coll)]
    (->> grouped
         (map (fn [[k vs]]
                {k (count vs)}))
         (apply merge))))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
  (pred item) returns true, including that item. pred must be
  free of side-effects. Returns a transducer when no collection
  is provided.

  [provided via Clojure project JIRA; non-mainlined patch]
  "
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (ensure-reduced (rf result input))
          (rf result input))))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s))))))))

(defn drop-nil
  "returns a lazy seq containing the non-nil elements of coll"
  [coll]
  (filter #(not (nil? %)) coll))

(defn split
  "invokes clojure.string/split with s an re. optionally, excludes
  instances of the empty string from the result of the split invocation.

  option: limit - defaults to nil
  option: remove-empty - defaults to false
  "
  [^CharSequence s ^Pattern re & {:keys [limit remove-empty]
                                  :or   {limit        nil
                                         remove-empty false}
                                  :as   options}]
  (let [tokens (if (nil? limit)
                 (clojure.string/split s re)
                 (clojure.string/split s re limit))]
    (if remove-empty
      (filter #(not= "" %) tokens)
      tokens)))

(defn re-get-named-group [^Pattern re s ^String group-name & group-names]
  (when-let [match (re-matcher re s)]
    (if (.find match)
      (let [keys (cons group-name group-names)]
        (->> keys
             (map (fn [^String x] [x (.group match x)]))
             (into {})))
      nil)))

(defn re-get-group [^Pattern re s ^String ^Integer group-index & group-indices]
  (when-let [match (re-matcher re s)]
    (if (.find match)
      (let [keys (cons group-index group-indices)]
        (->> keys
             (map (fn [^Integer x] [x (.group match x)]))
             (into {})))
      nil)))

(defn ->grid
  "creates a 2D grid (vector of vector) from the input string. 
   `project` is invoked for each character of the input and the resulting
   value will occupy the associated cell in the grid.
   
   `project` must be a function of arity 2:
     the first arg is the input character value,
     the second arg is a vector of length 2 representing the row index and column index.
   "
  ([ss project]
   (->> ss
        (str/split-lines)
        (map-indexed (fn [row-index row]
                       (->> row
                            (map-indexed (fn [col-index value]
                                           (project value [row-index col-index])))
                            (into []))))
        (into [])))
  ([ss]
   (->grid ss (fn [x [_row-index _col-index]]
                x))))

(defn find-in-grid
  "
  returns the coords in [row col] form for the first
  element of grid where the result of invoking predicate
  is true(thy)
  "
  [grid predicate]
  (->> (coords-2d grid)
       (some (fn [[row col :as coords]]
               (and (predicate (get-in grid [row col]))
                    coords)))))

(defonce ^:private grid-neighbor-transforms {:up         [1 0]
                                             :up-right   [1 1]
                                             :right      [0 1]
                                             :down-right [-1 1]
                                             :down       [-1 0]
                                             :down-left  [-1 -1]
                                             :left       [0 -1]
                                             :up-left    [1 -1]})

(defn neighbor-coords
  "
  returns a sequence of coordinate pairs for grid elements neighboring coords
  "
  [coords & {:keys [include-diagonal]
             :or   {include-diagonal false}}]
  (as-> [:up :right :down :left] $
    (if include-diagonal (concat $ [:up-right :down-right :down-left :up-left]) $)
    (map #(% grid-neighbor-transforms) $)
    (map #(m/add coords %) $)))

(defn neighbors [grid coords & {:keys [include-diagonal]
                                :or   {include-diagonal false}}]
  (->> (neighbor-coords coords include-diagonal)
       (map (fn [[row col]] (get-in grid [row col])))
       (filter some?)))