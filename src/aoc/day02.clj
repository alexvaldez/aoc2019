(ns aoc.day02
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn run [program]
  (loop [pgm program
         pc 0]
    (case (pgm pc)
      1 (recur (assoc pgm
                      (pgm (+ pc 3))
                      (+ (pgm (pgm (+ pc 1)))
                         (pgm (pgm (+ pc 2)))))
               (+ pc 4))
      2 (recur (assoc pgm
                      (pgm (+ pc 3))
                      (* (pgm (pgm (+ pc 1)))
                         (pgm (pgm (+ pc 2)))))
               (+ pc 4))
      99 pgm)))

(defn run-patch [input x1 x2]
  (let [patch (-> input
                  (assoc 1 x1)
                  (assoc 2 x2))
        output (run patch)]
    (output 0)))

(defn day02-1 [input]
  (run-patch input 12 2))

(defn day02-2 [input]
  (for [i (range (count input))
        j (range (count input))
        :when (= 19690720 (run-patch input i j))]
    (+ (* i 100) j)))

