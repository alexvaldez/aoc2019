(ns aoc.day19
  (:require [clojure.string :as str]
            [clojure.core.async :as async]
            [aoc.intcode :as ic]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn day19-1 [pgm]
  (apply +
         (map first
                (for [x (range 50) y (range 50)]
                  (ic/run pgm 512 [x y])))))

(defn day19-2 [pgm]
  (let [cin (async/chan 100)
        cout (async/chan 100)]
    (reduce (fn [n [x y]]
              (ic/exec (long-array 512 pgm) cin cout)
              (async/>!! cin x)
              (async/>!! cin y)
              (+ n (async/<!! cout)))
            0
            (for [x (range 100) y (range 100)] [x y]))))

