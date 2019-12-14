(ns aoc.day04
  (:require [clojure.string :as str]))

(defn valid1? [n]
  (let [s (mapv int (seq (str n)))]
    (and
      (empty? (filter (fn [i]
                        (> (s (dec i)) (s i)))
                      (range 1 (count s))))
      (seq (filter (fn [i]
                     (= (s (dec i)) (s i)))
                   (range 1 (count s)))))))

(defn valid2? [n]
  (let [s (mapv int (seq (str n)))]
    (and
      (empty? (filter (fn [i]
                        (> (s (dec i)) (s i)))
                      (range 1 (count s))))
      (seq (filter (fn [i]
                     (and (= (s (dec i)) (s i))
                          (or (= i (dec (count s)))
                              (not= (s i) (s (inc i))))
                          (or (= i 1)
                              (not= (s i) (s (- i 2))))))
                   (range 1 (count s)))))))

(defn day04-1 [n1 n2]
  (count (filter valid1? (range n1 (inc n2)))))

(defn day04-2 [n1 n2]
  (count (filter valid2? (range n1 (inc n2)))))
