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

(defn calc-quadrants [field [x0 y0]]
  [(reduce (fn [m [x y]]
             (update-in m
                        [(/ (- x x0) (- y0 y))]
                        (fnil conj (sorted-set-by (fn [p1 p2] (> (second p1) (second p2)))))
                        [x y]))
           (sorted-map)
           (filter (fn [[x y]] (and (>= x x0) (< y y0))) field))
   (reduce (fn [m [x y]]
             (update-in m
                        [(/ (- y y0) (- x x0))]
                        (fnil conj (sorted-set-by (fn [p1 p2] (< (first p1) (first p2)))))
                        [x y]))
           (sorted-map)
           (filter (fn [[x y]] (and (> x x0) (>= y y0))) field))
   (reduce (fn [m [x y]]
             (update-in m
                        [(/ (- x0 x) (- y y0))]
                        (fnil conj (sorted-set-by (fn [p1 p2] (< (second p1) (second p2)))))
                        [x y]))
           (sorted-map)
           (filter (fn [[x y]] (and (<= x x0) (> y y0))) field))
   (reduce (fn [m [x y]]
             (update-in m
                        [(/ (- y0 y) (- x0 x))]
                        (fnil conj (sorted-set-by (fn [p1 p2] (> (first p1) (first p2)))))
                        [x y]))
           (sorted-map)
           (filter (fn [[x y]] (and (< x x0) (<= y y0))) field))])

(defn count-viz [quadrants]
  (apply + (map count quadrants)))

(defn day10-1 [field]
  (apply max (map #(count-viz (calc-quadrants field %)) field)))

(defn get-targets [q]
  (if (empty? q)
    q
    (let [head (peek q)
          body (pop q)
          tail (rest head)]
      (if (empty? tail)
        body
        (conj body tail)))))

(defn day10-2 [field]
  (let [[maximum quadrants] (reduce (fn [[mx q] p]
                                      (let [quadrants (calc-quadrants field p)
                                            cv (count-viz quadrants)]
                                        (if (> cv mx)
                                          [cv quadrants]
                                          [mx q])))
                                    [0 nil]
                                    field)
        queue (into clojure.lang.PersistentQueue/EMPTY (apply concat (map vals quadrants)))]
    (nth (map (fn [q] (first (peek q))) (iterate get-targets queue)) 199)))


