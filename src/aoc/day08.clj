(ns aoc.day08
  (:require [clojure.string :as str]))

(defn parse [fname]
  (loop [v []
         s (str/split (str/trim (slurp fname)) #"")]
    (if (empty? s)
      v
      (recur (conj v (subvec s 0 150))
             (subvec s 150)))))

(defn calc-screen [input]
  (reduce (fn [screen layer]
            (mapv (fn [n]
                    (if (= "2" (screen n))
                      (layer n)
                      (screen n)))
                  (range 150)))
          (vec (repeat 150 "2")) 
          input))

(defn display [screen]
  (let [img (mapv #(get-in {"0" " " "1" "*" "2" "."} [%]) screen)]
    (println (subvec img 0 25))
    (println (subvec img 25 50))
    (println (subvec img 50 75))
    (println (subvec img 75 100))
    (println (subvec img 100 125))
    (println (subvec img 125 150))))

(defn day08-1 [input]
  (let [counted (map (fn [layer]
                       [(count (filter #(= "0" %) layer))
                        layer])
                     input)
        min-zero (first (sort-by first counted))
        count-1 (count (filter #(= "1" %) (second min-zero)))
        count-2 (count (filter #(= "2" %) (second min-zero)))]
    (* count-1 count-2)))

(defn day08-2 [input]
  (display (calc-screen input)))


