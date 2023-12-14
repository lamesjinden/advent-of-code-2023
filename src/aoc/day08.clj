(ns aoc.day08
  (:require [clojure.math.numeric-tower :as num]
            [instaparse.core :as insta]
            [aoc.util :as u]))

(def input-path "resources/day08.txt")

(def sample-input "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def sample-input2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defn parse-instructions [s]
  (->> s
       (vec)))

(def node-parser
  (insta/parser
    "<node> = id <#'\\s'>+ <'='> <#'\\s'>+ <'('> left <', '> right <')'>
    id = #'\\w+'
    left = #'\\w+'
    right = #'\\w+'"))

(defn parse-node [s]
  (let [node-data (node-parser s)
        [[_ id] [_ left] [_ right]] node-data]
    {:id    id
     :left  left
     :right right}))

(defn parse-network-map [ss]
  (->> ss
       (map parse-node)
       (apply u/index-by :id)))

(defn parse-network [input]
  (let [line-groups (u/split-on-newline input)
        [instructions-lines map-lines] line-groups
        instructions (parse-instructions (first instructions-lines))
        network-map (parse-network-map map-lines)]
    {:instructions instructions
     :network-map  network-map}))

(defn part1 []
  (let [{:keys [instructions network-map]} (parse-network (slurp input-path))]
    (->> (cycle instructions)
         (reduce (fn [{:keys [id steps] :as state} instruction]
                   (if (= id "ZZZ")
                     (reduced steps)
                     (let [node (get network-map id)
                           id' (if (= \R instruction)
                                 (:right node)
                                 (:left node))]
                       (-> state
                           (assoc :id id')
                           (update :steps inc)))))
                 {:id "AAA" :steps 0}))))

(def sample-input3 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn part2 []
  (let [{:keys [instructions network-map]} (parse-network (slurp input-path))
        starting-nodes (->> (vals network-map)
                            (filter #(.endsWith (:id %) "A")))
        ending-nodes (->> (vals network-map)
                          (filter #(.endsWith (:id %) "Z"))
                          (into #{}))
        walks (map (fn [starting-node]
                     (reduce (fn [state instruction]
                               (let [node (get state 0)
                                     steps (get state 1)]
                                 (if (contains? ending-nodes node)
                                   (reduced steps)
                                   (let [instruction-kw (if (= \R instruction)
                                                          :right
                                                          :left)
                                         node' (let [id (get node :id)
                                                     next-id (get-in network-map [id instruction-kw])]
                                                 (get network-map next-id))]
                                     [node' (inc steps)]))))
                             [starting-node 0]
                             (cycle instructions)))
                   starting-nodes)]
    (reduce num/lcm walks)))

