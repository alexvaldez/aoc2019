(ns aoc.day07
  (:require [clojure.string :as str]
            [clojure.core.async :as async]
            [aoc.intcode :as ic]))

(defn parse [s]
  (mapv #(Long/parseLong (str/trim %))
        (str/split (slurp s) #",")))

(defn perm [a]
  (if (empty? a)
    [[]]
    (let [v (vec a)]
      (reduce (fn [perms n]
                (let [elem (v n)]
                  (concat perms
                          (mapv #(conj % elem)
                                (perm (concat (subvec v 0 n)
                                              (subvec v (inc n))))))))
              []
              (range (count v))))))

(defn day07-1 [input]
  (apply max
         (map (fn [v]
                (reduce (fn [in n]
                          (first (ic/run input [n in])))
                        0
                        v))
              (perm [0 1 2 3 4]))))

(defn feedback [input v]
  (let [cha (async/chan 100)
        chb (async/chan 100)
        chc (async/chan 100)
        chd (async/chan 100)
        che (async/chan 100)]
    (async/>!! cha (v 0))
    (async/>!! chb (v 1))
    (async/>!! chc (v 2))
    (async/>!! chd (v 3))
    (async/>!! che (v 4))
    (async/>!! cha 0)
    (let [vma (ic/exec (long-array 100000 input) cha chb)
          vmb (ic/exec (long-array 100000 input) chb chc)
          vmc (ic/exec (long-array 100000 input) chc chd)
          vmd (ic/exec (long-array 100000 input) chd che)
          vme (ic/exec (long-array 100000 input) che cha)]
      (async/<!! vme))))

(defn day07-2 [input]
  (apply max
         (map (fn [v]
                (feedback input v))
              (perm [5 6 7 8 9]))))

