(ns day-10-test (:require [clojure.test :refer :all]
                          [day-10 :refer [find-max-from-map]]))

(deftest day10-1
  (let [a-map (str
               ".#..#\n"
               ".....\n"
               "#####\n"
               "....#\n"
               "...##\n")]
    (is (= (find-max-from-map a-map) 8))))

(deftest day10-2
  (let [a-map (str
               "......#.#.\n"
               "#..#.#....\n"
               "..#######.\n"
               ".#.#.###..\n"
               ".#..#.....\n"
               "..#....#.#\n"
               "#..#....#.\n"
               ".##.#..###\n"
               "##...#..#.\n"
               ".#....####\n")]
    (is (= (find-max-from-map a-map) 33))))