(ns aoc.day06
  (:require [clojure.math :as math]
            [clojure.string :as s]
            [clojure.set :as set]
            [instaparse.core :as insta]
            [aoc.util :as u]))

(def input-path "resources/day06.txt")

(def sample-input "Time:      7  15   30
Distance:  9  40  200")

(def time-parser
  (insta/parser
    "<time> = <'Time:'> <#'\\s'>+ numbers
     numbers = #'\\d+' (<#'\\s'>+ #'\\d+')*"))

(defn parse-time [line]
  (time-parser line))

(def distance-parser
  (insta/parser
    "<distance> = <'Distance:'> <#'\\s'>+ numbers
     numbers = #'\\d+' (<#'\\s'>+ #'\\d+')*"))

(defn parse-distance [line]
  (distance-parser line))

(defn distance [rate time]
  (* rate time))

(defn no-ways-to-beat-record [time record]
  (->> (range 1 time)
       (map (fn [charge] (distance charge (- time charge))))
       (filter #(> % record))))

(defn part1 []
  (let [lines (-> (slurp input-path)
                  (s/split-lines))
        time-line (first lines)
        [_ & times] (first (parse-time time-line))
        distance-line (second lines)
        [_ & distances] (first (parse-distance distance-line))
        race-time->record (zipmap (map parse-long times) (map parse-long distances))]
    (->> race-time->record
         (map (fn [[time record]]
                (no-ways-to-beat-record time record)))
         (map count)
         (apply *))))

(defn part2 []
  (let [lines (-> (slurp input-path)
                  (s/split-lines))
        time-line (first lines)
        [_ & times] (first (parse-time time-line))
        distance-line (second lines)
        [_ & distances] (first (parse-distance distance-line))
        race-time->record {(parse-long (apply str (map parse-long times)))
                           (parse-long (apply str (map parse-long distances)))}]
    (->> race-time->record
         (map (fn [[time record]]
                (no-ways-to-beat-record time record)))
         (map count))))
