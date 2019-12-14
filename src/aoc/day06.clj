(ns aoc.day06
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv #(str/split % #"\)")
        (str/split-lines (slurp s))))

(defn make-tree [input]
  (reduce (fn [m [k v]]
            (update-in m [k] #(conj % v)))
          {}
          input))

(defn count-orbit [m depth obj]
  (println (m obj))
  (+ depth
     (apply + (mapv #(count-orbit m (inc depth) %) (m obj)))))

(defn make-graph [input]
  (reduce (fn [m [v k]]
            (assoc m k v))
          {}
          input))

(defn trace-path [m k]
  (if (contains? m k)
    (conj (trace-path m (m k)) k)
    [k]))

(defn day06-1 [input]
  (count-orbit (make-tree input) 0 "COM"))

(defn day06-2 [input]
  (let [m (make-graph input)
        you (trace-path m "YOU")
        san (trace-path m "SAN")
        n (first (filter (fn [n] (not= (you n) (san n))) (range)))]
    (+ (- (count you) n 1) (- (count san) n 1))))

