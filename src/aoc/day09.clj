(ns aoc.day09
  (:require [clojure.string :as str]
            [aoc.intcode :as ic]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn day09-1 [pgm]
  (ic/run pgm 40000000 [1]))

(defn day09-2 [pgm]
  (ic/run pgm 40000000 [2]))
