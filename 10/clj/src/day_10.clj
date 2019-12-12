(ns day-10 (:import java.math.MathContext))

(comment
  (def asteroid-map
    (str
     ".#..#\n"
     ".....\n"
     "#####\n"
     "....#\n"
     "...##\n"))
  (def asteroids (filter-asteroids (map-to-points asteroid-map)))
  (def start {:coords [1 0] :asteroid? true}))

(defn row-to-points [y row]
  (map-indexed (fn [index symbol]
                 {:coords [index y]
                  :asteroid? (= symbol \#)})
               row))

(defn map-to-points [astroid-map]
  (flatten
   (map-indexed row-to-points
                (clojure.string/split-lines astroid-map))))

(defn filter-asteroids [points]
  (filter :asteroid? points))

(defn get-unit-vec [start target]
  (with-precision 5
    :rounding FLOOR
    (let [{[x1 y1] :coords} start
          {[x2 y2] :coords} target
          vx (bigdec (- x2 x1))
          vy (bigdec (- y2 y1))
          vmag (.sqrt (+ (* vx vx) (* vy vy)) MathContext/DECIMAL32)]
      [(/ vx vmag) (/ vy vmag)])))

(defn count-los [asteroids start]
  (->>
   asteroids
   (filter (partial not= start))
   (map (partial get-unit-vec start))
   (into #{})
   (count)))

(defn get-all-counts [asteroids]
  (map (partial count-los asteroids) asteroids))

(defn find-max-from-map [asteroid-map]
  (->>
   asteroid-map
   (map-to-points)
   (filter-asteroids)
   (get-all-counts)
   (apply max)))

(defn part-one []
  (find-max-from-map (slurp "../input.txt")))