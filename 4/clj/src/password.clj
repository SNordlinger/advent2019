(ns password)

(defn split-digits [number]
  (reverse (map #(rem % 10) (take-while pos? (iterate #(quot % 10) number)))))

(defn is-incresing? [digits]
  (= digits (sort digits)))

(defn is-repeating [digits]
  (some true? (map = digits (cons nil digits))))

(defn double-repeat [digits]
  (some (fn [[_ freq]] (= freq 2)) (frequencies digits)))

(defn number-valid-part1 [number]
  (let [digits (split-digits number)]
    (and (is-incresing? digits) (is-repeating digits))))

(defn number-valid-part2 [number]
  (let [digits (split-digits number)]
    (and (is-incresing? digits) (double-repeat digits))))

(defn part1 []
  (count (filter number-valid-part1 (range 125730 579381))))

(defn part2 []
  (count (filter number-valid-part2 (range 125730 579381))))

(defn -main []
  (println (str "Part 1: " (part1)))
  (println (str "Part 2: " (part2))))