(ns day-11.robot (:require [day-11.comp :as intcode]))

(def position-data {:dir 90
                    :coords [0 0]})

(def painted-squares {})

(defn update-position
  [position-data turn]
  (let [[x y] (position-data :coords)
        old-dir (position-data :dir)
        new-dir (if (> turn 0)
                  (mod (+ old-dir 90) 360)
                  (mod (+ old-dir 270) 360))]
    (assoc position-data
           :dir new-dir
           :coords (case new-dir
                     0 [x (+ y 1)]
                     90 [(+ x 1) y]
                     180 [x (- y 1)]
                     270 [(- x 1) y]))))

(defn get-current-color
  [position-data painted-squares]
  (let [painted-color (painted-squares (position-data :coords))]
    (if (= painted-color 1) 1 0)))

(first (filter #(= 2 (count (% :output)))
               (iterate intcode/run-cycle
                        (intcode/initial-state (intcode/get-program) [0]))))