(ns aoc.day01
  (:require [clojure.string :as s]
            [aoc.util :as u]))

(def sample-input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")

(defn str->digits [^String s]
  (let [first-digit-char #(and (Character/isDigit ^char %) %)
        f (some first-digit-char s)
        l (some first-digit-char (reverse s))]
    [f l]))

(defn digits->number [[first last]]
  (-> (str first last)
      (u/->int)))

(def str->number (comp digits->number str->digits))

(defn part1 []
  (->> (slurp "resources/day01.txt")
       (s/split-lines)
       (map str->number)
       (reduce +)))

(comment

  (s/split-lines sample-input)
  (first (s/split-lines sample-input))
  (str->digits (first (s/split-lines sample-input)))
  (str->digits (second (s/split-lines sample-input)))

  (str->digits "1abc2")
  (str->number "1abc2")

  (let [lines (s/split-lines sample-input)]
    (str->number (get lines 0))
    (str->number (get lines 1))
    (str->number (get lines 2))
    (str->number (get lines 3)))

  (->> (s/split-lines sample-input)
       (map str->number)
       (reduce +))

  (part1)
  )

(def sample-input2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")

(def spelled-digits {"one"   1
                     "two"   2
                     "three" 3
                     "four"  4
                     "five"  5
                     "six"   6
                     "seven" 7
                     "eight" 8
                     "nine"  9})

(defn str->digits2 [^String s]
  (let [needles (into #{} (concat (keys spelled-digits) (map str (vals spelled-digits))))
        found-at (->> needles
                      (map #(hash-map :needle %
                                      :first-index (.indexOf s ^String %)
                                      :last-index (.lastIndexOf s ^String %))))
        f (:needle (apply min-key :first-index (filter #(>= (:first-index %) 0) found-at)))
        l (:needle (apply max-key :last-index (filter #(>= (:last-index %) 0) found-at)))]
    [(or (get spelled-digits f) (u/->int f))
     (or (get spelled-digits l) (u/->int l))]))

(def str->number2 (comp digits->number str->digits2))

(defn part2 []
  (->> (slurp "resources/day01.txt")
       (s/split-lines)
       (map str->number2)
       (reduce +)))

(comment

  (->> sample-input2
       (s/split-lines)
       (take 1)
       (map str->digits2))

  (->> (s/split-lines sample-input2)
       (map str->number2)
       (reduce +))

  (part2)

  )
