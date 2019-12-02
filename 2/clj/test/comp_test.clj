(ns comp-test (:require [comp :refer [run]]
                        [clojure.test :refer :all]))

(deftest part1
  (is (= (run [1 0 0 0 99]) [2 0 0 0 99]))
  (is (= (run [2 3 0 3 99]) [2 3 0 6 99]))
  (is (= (run [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
  (is (= (run [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99])))