(ns aoc.day10
  (:require [clojure.string :as str]))

(defn parse [s]
  (let [lines (str/split-lines (slurp s))]
    (reduce (fn [m y]
              (let [chars (vec (lines y))]
                (reduce (fn [m x]
                          (if (= \# (chars x))
                            (conj m [x y])
                            m))
                        m
                        (range (count chars)))))
            []
            (range (count lines)))))

(defn count-viz [field [x0 y0]]
  (+ (count (reduce (fn [m [x y]]
                      (update-in m
                                 [(/ (- x x0) (- y y0))]
                                 (fnil conj #{}) [x y]))
                    {}
                    (filter (fn [[x y]] (and (>= x x0) (> y y0))) field)))
     (count (reduce (fn [m [x y]]
                      (update-in m
                                 [(/ (- y y0) (- x x0))]
                                 (fnil conj #{}) [x y]))
                    {}
                    (filter (fn [[x y]] (and (< x x0) (>= y y0))) field)))
     (count (reduce (fn [m [x y]]
                      (update-in m
                                 [(/ (- x x0) (- y y0))]
                                 (fnil conj #{}) [x y]))
                    {}
                    (filter (fn [[x y]] (and (<= x x0) (< y y0))) field)))
     (count (reduce (fn [m [x y]]
                      (update-in m
                                 [(/ (- y y0) (- x x0))]
                                 (fnil conj #{}) [x y]))
                    {}
                    (filter (fn [[x y]] (and (> x x0) (<= y y0))) field)))))

(defn day10-1 [field]
  (apply max (map #(count-viz field %) field)))
