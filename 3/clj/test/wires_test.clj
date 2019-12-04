(ns wires-test (:require [clojure.test :refer :all]
                         [wires :refer [closest-point shortest-length paths-from-inst parse-inst-line]]))

(deftest part1-example1
  (let [instructions [["R8" "U5" "L5" "D3"]
                      ["U7" "R6" "D4" "L4"]]]
    (is (= (apply closest-point (paths-from-inst  instructions)) 6))))

(deftest part1-example2
  (let [instructions [(parse-inst-line "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                      (parse-inst-line "U62,R66,U55,R34,D71,R55,D58,R83")]]
    (is (= (apply closest-point (paths-from-inst  instructions)) 159))))

(deftest part2-example1
  (let [instructions [["R8" "U5" "L5" "D3"]
                      ["U7" "R6" "D4" "L4"]]]
    (is (= (apply shortest-length (paths-from-inst  instructions)) 30))))

(deftest part2-example2
  (let [instructions [(parse-inst-line "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                      (parse-inst-line "U62,R66,U55,R34,D71,R55,D58,R83")]]
    (is (= (apply shortest-length (paths-from-inst  instructions)) 610))))