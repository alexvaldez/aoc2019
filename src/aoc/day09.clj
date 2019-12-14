(ns aoc.day09
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn run [program input]
  (let [ram (long-array 40000000 program)]
    (loop [pc 0
           base 0
           in input
           out []]
      (defn memset [addr mode n]
        (case mode
          0 (aset ram (aget ram addr) n)
          1 (aset ram addr n)
          2 (aset ram (+ base (aget ram addr)) n)))
      (defn memget [addr mode]
        (case mode
          0 (aget ram (aget ram addr))
          1 (aget ram addr)
          2 (aget ram (+ base (aget ram addr)))))
      (let [op (aget ram pc)
            opcode (mod op 100)
            mode1 (int (mod (/ op 100) 10))
            mode2 (int (mod (/ op 1000) 10))
            mode3 (int (mod (/ op 10000) 10))]
        (case opcode
          1 (recur (do (memset (+ pc 3) mode3
                               (+ (memget (+ pc 1) mode1)
                                  (memget (+ pc 2) mode2)))
                       (+ pc 4))
                   base
                   in
                   out)
          2 (recur (do (memset (+ pc 3) mode3
                               (* (memget (+ pc 1) mode1)
                                  (memget (+ pc 2) mode2)))
                       (+ pc 4))
                   base
                   in
                   out)
          3 (recur (do (memset (+ pc 1) mode1
                               (first in))
                       (+ pc 2))
                   base
                   (rest in)
                   out)
          4 (recur (+ pc 2)
                   base
                   in
                   (conj out (memget (+ pc 1) mode1)))
          5 (recur (if (not= 0 (memget (+ pc 1) mode1))
                     (memget (+ pc 2) mode2)
                     (+ pc 3))
                   base
                   in
                   out)
          6 (recur (if (= 0 (memget (+ pc 1) mode1))
                     (memget (+ pc 2) mode2)
                     (+ pc 3))
                   base
                   in
                   out)
          7 (recur (do (memset (+ pc 3) mode3
                               (if (< (memget (+ pc 1) mode1)
                                      (memget (+ pc 2) mode2))
                                 1
                                 0))
                       (+ pc 4))
                   base
                   in
                   out)
          8 (recur (do (memset (+ pc 3) mode3
                               (if (= (memget (+ pc 1) mode1)
                                      (memget (+ pc 2) mode2))
                                 1
                                 0))
                       (+ pc 4))
                   base
                   in
                   out)
          9 (recur (+ pc 2)
                   (+ base (memget (+ pc 1) mode1))
                   in
                   out)
          99 out)))))

(defn day09-1 [pgm]
  (run pgm [1]))

(defn day09-2 [pgm]
  (run pgm [2]))
