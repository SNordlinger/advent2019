(ns wires (:require [clojure.set]))

(defn inst-path [start move axis]
  (let [distance (Math/abs move)
        op (if (pos? move) + -)]
    (map (fn [d] (assoc start axis (op (start axis) d)))
         (range 1 (+ distance 1)))))

(defn apply-inst [start inst]
  (let [dir (first inst)
        distance (Integer/parseInt (subs inst 1))]
    (case dir
      \R (inst-path start distance :x)
      \L (inst-path start (- distance) :x)
      \U (inst-path start distance :y)
      \D (inst-path start (- distance) :y))))

(defn get-path [start instructions]
  (reduce (fn [path inst] (concat path (apply-inst (last path) inst)))
          [start]
          instructions))

(defn path-intersection [path1 path2]
  (clojure.set/intersection (set (rest path1))
                            (set (rest path2))))

(defn closest-point [path1 path2]
  (let [intersections (path-intersection path1 path2)]
    (apply min (map (fn [{x :x y :y}] (+ (Math/abs x) (Math/abs y))) intersections))))

(defn parse-inst-line [line]
  (clojure.string/split line #","))

(defn get-instructions []
  (->>
   (slurp "../input.txt")
   clojure.string/split-lines
   (map parse-inst-line)))

(defn paths-from-inst [inst-lines]
  (->>
   inst-lines
   (map (partial get-path {:x 0 :y 0}))))

(defn wire-distance [path1 path2 point]
  (+ (count (take-while (partial not= point) path1))
     (count (take-while (partial not= point) path2))))

(defn shortest-length [path1 path2]
  (let [intersections (path-intersection path1 path2)]
    (apply min (map (partial wire-distance path1 path2) intersections))))

(defn part1 []
  (apply closest-point (paths-from-inst (get-instructions))))

(defn part2 []
  (apply shortest-length (paths-from-inst (get-instructions))))

(defn -main []
  (println (str "Part 1: " (part1)))
  (println (str "Part 2: " (part2))))