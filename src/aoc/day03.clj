(ns aoc.day03
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (mapv (fn [s]
            [(first s) (Integer/parseInt (subs s 1))])
        (str/split line #",")))

(defn parse [s]
  (mapv parse-line
        (str/split-lines (slurp s))))

(defn to-lines [v]
  (let [[x y lines]
        (reduce (fn [[x y lines] [dir len]]
                  (case dir
                    \R [(+ x len)
                        y
                        (conj lines [x y (+ x len) y])]
                    \L [(- x len)
                        y
                        (conj lines [x y (- x len) y])]
                    \U [x
                        (+ y len)
                        (conj lines [x y x (+ y len)])]
                    \D [x
                        (- y len)
                        (conj lines [x y x (- y len)])]))
                [0 0 []]
                v)]
    lines))

(defn intersects? [[p1x1 p1y1 p1x2 p1y2]
                   [p2x1 p2y1 p2x2 p2y2]]
  (not (or (< (max p1x1 p1x2) (min p2x1 p2x2))
           (< (max p2x1 p2x2) (min p1x1 p1x2))
           (< (max p1y1 p1y2) (min p2y1 p2y2))
           (< (max p2y1 p2y2) (min p1y1 p1y2)))))

(defn manhattan [[p1x1 p1y1 p1x2 p1y2]
                 [p2x1 p2y1 p2x2 p2y2]]
  (+ (Math/abs (max (min p1x1 p1x2) (min p2x1 p2x2)))
     (Math/abs (max (min p1y1 p1y2) (min p2y1 p2y2)))))

(defn count-steps [lines]
  (reduce (fn [steps [x1 y1 x2 y2]]
            (+ steps
               (Math/abs (- x2 x1))
               (Math/abs (- y2 y1))))
           0
           lines))
(defn total-steps [lines1 lines2 n1 n2]
  (let [[p1x1 p1y1 p1x2 p1y2] (lines1 n1)
        [p2x1 p2y1 p2x2 p2y2] (lines2 n2)
        x (max (min p1x1 p1x2) (min p2x1 p2x2))
        y (max (min p1y1 p1y2) (min p2y1 p2y2))]
    (+ (count-steps (take n1 lines1))
       (count-steps (take n2 lines2))
       (Math/abs (- x p1x1))
       (Math/abs (- y p1y1))
       (Math/abs (- x p2x1))
       (Math/abs (- y p2y1)))))

(defn day03-1 [[input1 input2]]
  (let [lines1 (to-lines input1)
        lines2 (to-lines input2)]
    (apply min
           (filter pos? 
                   (for [p1 lines1
                         p2 lines2
                         :when (intersects? p1 p2)]
                     (manhattan p1 p2))))))

(defn day03-2 [[input1 input2]]
  (let [lines1 (to-lines input1)
        lines2 (to-lines input2)]
    (apply min
           (filter pos? 
                   (for [n1 (range (count lines1))
                         n2 (range (count lines2))
                         :when (intersects? (lines1 n1) (lines2 n2))]
                     (total-steps lines1 lines2 n1 n2))))))
