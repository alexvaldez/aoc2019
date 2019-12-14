(ns aoc.day12
  (:require [clojure.string :as str]))

(defn parse [fname]
  (map (fn [line] 
         (map #(Integer/parseInt %) (re-seq #"-?\d+" line)))
       (str/split-lines (slurp fname))))

(defn step [moons]
  (->> moons
       (mapv (fn [moon]
               (reduce (fn [[x y z dx dy dz] [x' y' z' dx' dy' dz']]
                         [x y z
                          (+ dx (Long/signum (- x' x)))
                          (+ dy (Long/signum (- y' y)))
                          (+ dz (Long/signum (- z' z)))])
                       moon
                       moons)))
       (mapv (fn [[x y z dx dy dz]]
               [(+ x dx) (+ y dy) (+ z dz) dx dy dz]))))

(defn energy [moons]
  (apply + (map (fn [moon]
                  (let [abs (map #(Math/abs %) moon)]
                    (* (apply + (take 3 abs))
                       (apply + (drop 3 abs)))))
                moons)))

(defn gcm
  ([a b]
   (let [d (mod a b)]
     (if (= 0 d)
       b
       (gcm b d))))
  ([a b & c]
   (reduce gcm (gcm a b) c)))

(defn lcm 
  ([a b]
   (/ (* a b) (gcm a b)))
  ([a b & c]
   (reduce lcm (lcm a b) c)))

(defn day09-1 [input]
  (let [moons (mapv (fn [v] (into [] (concat v [0 0 0]))) input)
        end-state (nth (iterate step moons) 1000)]
    (energy end-state)))


(defn day09-2 [input]
  (let [moons (mapv (fn [v] (into [] (concat v [0 0 0]))) input)]
    ;; check repeats of each of x y and z
    ))
