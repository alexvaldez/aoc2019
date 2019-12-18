(ns aoc.intcode
  (:require [clojure.core.async :as async]))

  (defn memset [ram addr mode base n]
    (case mode
      0 (aset ram (aget ram addr) n)
      1 (aset ram addr n)
      2 (aset ram (+ base (aget ram addr)) n)))
  (defn memget [ram addr mode base]
    (case mode
      0 (aget ram (aget ram addr))
      1 (aget ram addr)
      2 (aget ram (+ base (aget ram addr)))))

(defn exec [ram cin cout]
  (async/go
    (loop [pc 0
           base 0
           rc 0]
      (let [op (aget ram pc)
            opcode (mod op 100)
            mode1 (int (mod (/ op 100) 10))
            mode2 (int (mod (/ op 1000) 10))
            mode3 (int (mod (/ op 10000) 10))]
        (case opcode
          ; add
          1 (do
              (memset ram (+ pc 3) mode3 base
                      (+ (memget ram (+ pc 1) mode1 base)
                         (memget ram (+ pc 2) mode2 base)))
              (recur (+ pc 4)
                     base
                     rc))
          ; multiply
          2 (do
              (memset ram (+ pc 3) mode3 base
                      (* (memget ram (+ pc 1) mode1 base)
                         (memget ram (+ pc 2) mode2 base)))
              (recur (+ pc 4)
                     base
                     rc))
          ; input
          3 (do
              (memset ram (+ pc 1) mode1 base
                      (async/<! cin))
              (recur (+ pc 2)
                     base
                     rc))
          ; output
          4 (let [output (memget ram (+ pc 1) mode1 base)]
              (async/>! cout output)
              (recur (+ pc 2)
                     base
                     output))
          ; jump on non-zero
          5 (recur (if (not= 0 (memget ram (+ pc 1) mode1 base))
                     (memget ram (+ pc 2) mode2 base)
                     (+ pc 3))
                   base
                   rc)
          ; jump on zero
          6 (recur (if (= 0 (memget ram (+ pc 1) mode1 base))
                     (memget ram (+ pc 2) mode2 base)
                     (+ pc 3))
                   base
                   rc)
          ; test less than
          7 (do
              (memset ram (+ pc 3) mode3 base
                      (if (< (memget ram (+ pc 1) mode1 base)
                             (memget ram (+ pc 2) mode2 base))
                        1
                        0))
              (recur (+ pc 4)
                     base
                     rc))
          ; test equals
          8 (do (memset ram (+ pc 3) mode3 base
                        (if (= (memget ram (+ pc 1) mode1 base)
                               (memget ram (+ pc 2) mode2 base))
                          1
                          0))
                (recur (+ pc 4)
                       base
                       rc))
          ; set base
          9 (recur (+ pc 2)
                   (+ base (memget ram (+ pc 1) mode1 base))
                   rc)
          ; exit
          99 rc)))))

(defn collect [ch]
    (async/go 
      (reduce (fn [outputs output]
                (if (nil? output)
                  (reduced outputs)
                  (conj outputs output)))
              []
              (repeatedly #(async/<!! ch)))))

(defn run 
  ([pgm inputs]
   (run pgm (count pgm) inputs))
  ([pgm memsize inputs]
   (let [ram (long-array memsize pgm)
         cin (async/chan (count inputs))
         cout (async/chan 100)
         cexec (exec ram cin cout)
         ccol (collect cout)]
     (doseq [n inputs]
       (async/>!! cin n))
     (async/<!! cexec)
     (async/close! cin)
     (async/close! cout)
     (async/<!! ccol))))
