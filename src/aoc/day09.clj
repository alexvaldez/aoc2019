(ns aoc.day09
  (:require [clojure.string :as str]
            [clojure.core.async :as async]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn exec [ram cin cout]
  (async/go
    (loop [pc 0
           base 0]
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
                   base)
          2 (recur (do (memset (+ pc 3) mode3
                               (* (memget (+ pc 1) mode1)
                                  (memget (+ pc 2) mode2)))
                       (+ pc 4))
                   base)
          3 (recur (do (memset (+ pc 1) mode1
                               (async/<! cin))
                       (+ pc 2))
                   base)
          4 (recur (do (async/>! cout (memget (+ pc 1) mode1))
                       (+ pc 2))
                   base)
          5 (recur (if (not= 0 (memget (+ pc 1) mode1))
                     (memget (+ pc 2) mode2)
                     (+ pc 3))
                   base)
          6 (recur (if (= 0 (memget (+ pc 1) mode1))
                     (memget (+ pc 2) mode2)
                     (+ pc 3))
                   base)
          7 (recur (do (memset (+ pc 3) mode3
                               (if (< (memget (+ pc 1) mode1)
                                      (memget (+ pc 2) mode2))
                                 1
                                 0))
                       (+ pc 4))
                   base)
          8 (recur (do (memset (+ pc 3) mode3
                               (if (= (memget (+ pc 1) mode1)
                                      (memget (+ pc 2) mode2))
                                 1
                                 0))
                       (+ pc 4))
                   base)
          9 (recur (+ pc 2)
                   (+ base (memget (+ pc 1) mode1)))
          99 "exit")))))

(defn run [pgm n]
  (let [ram (long-array 40000000 pgm)
        cin (async/chan 100)
        cout (async/chan 100)
        c (exec ram cin cout)]
    (async/>!! cin n)
    (async/go 
      (while (let [x (async/<!! cout)]
               (if x (println x))
               x)))
    (async/<!! c)
    (async/close! cin)
    (async/close! cout)))


(defn day09-1 [pgm]
  (run pgm 1))

(defn day09-2 [pgm]
  (run pgm 2))
