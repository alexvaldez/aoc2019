(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv #(Long/parseLong %)
        (str/split-lines (slurp s))))

(defn calc-fuel [mass]
  (-> mass (/ 3) (int) (- 2)))

(defn calc-total-fuel [mass]
  (apply + (take-while pos? (iterate calc-fuel (calc-fuel mass)))))

(defn day01-1 [input]
  (apply + (mapv calc-fuel input)))

(defn day01-2 [input]
  (apply + (mapv calc-total-fuel input)))
