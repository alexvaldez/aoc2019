(ns aoc.day23
  (:require [clojure.string :as str]
            [clojure.core.async :as async]
            [aoc.intcode :as ic]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn setup-channels [pgm]
  (reduce (fn [chans n]
            (let [chin (async/chan 100)
                  chout (async/chan 100)]
              (ic/exec (long-array (+ (count pgm) 20000) pgm) chin chout)
              (async/>!! chin n)
              (conj chans [n chin chout])))
          []
          (range 50)))

(defn day23-1 [pgm]
  (let [chs (setup-channels pgm)]
    (reduce (fn [v [n chin chout]]
              ; (println "sending -1 to" n)
              (async/>!! chin -1)
              (let [dest (async/poll! chout)]
                (if dest
                  (let [x (async/<!! chout)
                        y (async/<!! chout)]
                    ; (println "output from" n dest x y)
                    (if (= dest 255)
                      (reduced [x y])
                      (do
                        (let [[i destin destout] (chs dest)]
                          (async/>!! destin x)
                          (async/>!! destin y)))))
                  [])))
            []
            (cycle chs))))

(defn day23-2 [pgm]
  (let [chs (setup-channels pgm)]
    (loop [natmem nil
           lasty nil]
      (let [[idle natlast]
            (reduce (fn [[idle natlast] [n chin chout]]
                      (println "sending -1 to" n)
                      (async/>!! chin -1)
                      (let [dest (async/poll! chout)]
                        (if dest
                          (let [x (async/<!! chout)
                                y (async/<!! chout)]
                            (println "output from" n dest x y)
                            (if (= dest 255)
                              (do
                                (println "sending to nat" [x y])
                                [false [x y]])
                              (do
                                (let [[i destin destout] (chs dest)]
                                  (async/>!! destin x)
                                  (async/>!! destin y))
                                [false natlast])))
                          [idle natlast])))
                    [true natmem]
                    chs)]
        (println "idle" idle "natlast" natlast)
        (if idle
          (if natlast
            (if (= (second natlast) lasty)
              lasty
              (let [[n0 chin0 chout0] (chs 0)]
                (async/>!! chin0 (first natlast))
                (async/>!! chin0 (second natlast))
                (recur natlast (second natlast))))
            (recur natlast lasty))
          (recur natlast lasty))))))

