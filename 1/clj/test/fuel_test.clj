(ns fuel-test (:require [clojure.test :refer :all]
                        [fuel :refer [fuel-req total-req]]))

(deftest part1
  (is (= (fuel-req 12) 2.0))
  (is (= (fuel-req 14) 2.0))
  (is (= (fuel-req 1969) 654.0))
  (is (= (fuel-req 100756) 33583.0)))

(deftest part2
  (is (= (total-req 14) 2.0))
  (is (= (total-req 1969) 966.0))
  (is (= (total-req 100756) 50346.0)))