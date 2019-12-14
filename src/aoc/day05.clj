(ns aoc.day05
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn run [program input]
  (loop [pgm program
         pc 0
         in input
         out []]
    (defn
         derf [addr mode]
                 (case mode
                   0 (pgm (pgm addr))
                   1 (pgm addr)))
    (let [op (pgm pc)
          opcode (mod op 100)
          mode1 (int (mod (/ op 100) 10))
          mode2 (int (mod (/ op 1000) 10))
          mode3 (int (mod (/ op 10000) 10))]
      (case opcode
        1 (recur (assoc pgm
                        (pgm (+ pc 3))
                        (+ (derf (+ pc 1) mode1)
                           (derf (+ pc 2) mode2)))
                 (+ pc 4)
                 in
                 out)
        2 (recur (assoc pgm
                        (pgm (+ pc 3))
                        (* (derf (+ pc 1) mode1)
                           (derf (+ pc 2) mode2)))
                 (+ pc 4)
                 in
                 out)
        3 (recur (assoc pgm
                        (pgm (+ pc 1))
                        (first in))
                 (+ pc 2)
                 (rest in)
                 out)
        4 (recur pgm
                 (+ pc 2)
                 in
                 (conj out (derf (+ pc 1) mode1)))
        5 (recur pgm
                 (if (not= 0 (derf (+ pc 1) mode1))
                   (derf (+ pc 2) mode2)
                   (+ pc 3))
                 in
                 out)
        6 (recur pgm
                 (if (= 0 (derf (+ pc 1) mode1))
                   (derf (+ pc 2) mode2)
                   (+ pc 3))
                   in
                   out)
        7 (recur (assoc pgm
                        (pgm (+ pc 3))
                        (if (< (derf (+ pc 1) mode1)
                               (derf (+ pc 2) mode2))
                          1
                          0))
                 (+ pc 4)
                 in
                 out)
        8 (recur (assoc pgm
                        (pgm (+ pc 3))
                        (if (= (derf (+ pc 1) mode1)
                               (derf (+ pc 2) mode2))
                          1
                          0))
                 (+ pc 4)
                 in
                 out)
        99 out))))

(defn day05-1 [pgm]
  (run pgm [1]))

(defn day05-2 [pgm]
  (run pgm [5]))
