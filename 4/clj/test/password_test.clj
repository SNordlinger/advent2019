(ns password-test (:require [clojure.test :refer :all]
                            [password :refer [number-valid-part1? number-valid-part2?]]))

(deftest part1
  (is (number-valid-part1? 111111))
  (is (not (number-valid-part1? 223450)))
  (is (not (number-valid-part1? 123789))))

(deftest part2
  (is (number-valid-part2? 112233))
  (is (not (number-valid-part2? 123444)))
  (is (number-valid-part2? 111122)))