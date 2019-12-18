(ns aoc.day05
  (:require [clojure.string :as str]
            [aoc.intcode :as ic]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn day05-1 [pgm]
  (ic/run pgm [1]))

(defn day05-2 [pgm]
  (ic/run pgm [5]))
