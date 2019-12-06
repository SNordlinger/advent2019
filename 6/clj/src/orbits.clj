(ns orbits (:require clojure.set
                     clojure.string))

(defn parse-listing [orbit-listing]
  (clojure.string/split orbit-listing #"\)"))

(defn map-from-listings [orbit-listings]
  (->>
   orbit-listings
   clojure.string/split-lines
   (map parse-listing)
   (map #(into [] (rseq %)))
   (into {})))

(defn get-orbits [orbit-map body]
  (take-while some? (iterate orbit-map body)))

(defn count-orbits [orbit-map body]
  (- (count (get-orbits orbit-map body)) 1))

(defn count-from-listings [orbit-listings]
  (let [orbit-map (map-from-listings orbit-listings)]
    (reduce + (map (partial count-orbits orbit-map)
                   (keys orbit-map)))))

(defn get-path-length [orbit-map]
  (let
   [you-orbits (set (get-orbits orbit-map "YOU"))
    san-orbits (set (get-orbits orbit-map "SAN"))]
    (+ (count (clojure.set/difference you-orbits san-orbits))
       (count (clojure.set/difference san-orbits you-orbits))
       -2)))

(defn part-one []
  (count-from-listings (slurp "../input.txt")))

(defn part-two []
  (->>
   (slurp "../input.txt")
   (map-from-listings)
   (get-path-length)))

(defn -main []
  (println (str "Part 1: " (part-one)))
  (println (str "Part 2: " (part-two))))