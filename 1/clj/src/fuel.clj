(ns fuel)

(defn get-masses []
  (map #(Integer/parseInt %)
       (clojure.string/split
        (slurp "../input.txt")
        #"\n")))

(defn fuel-req [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn total-req [mass]
  (let [req (fuel-req mass)]
    (if (<= req 0)
      0
      (+ req (total-req req)))))

(defn part-one []
  (->>
   (get-masses)
   (map fuel-req)
   (apply +)))

(defn part-two []
  (->>
   (get-masses)
   (map total-req)
   (apply +)))

(defn -main []
  (println (str "Part 1: " (part-one)))
  (println (str "Part 2: " (part-two))))