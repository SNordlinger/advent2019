(ns decoder (:require clojure.string))

(def image-size (* 25 6))
(def image-width 25)

(defn get-encoded-image []
  (->>
   (slurp "../input.txt")
   clojure.string/trim-newline
   (map #(Character/digit % 10))))

(defn get-color [& pixel]
  (first (filter (partial not= 2) pixel)))

(defn get-pixel [pixel-val]
  (if (= pixel-val 1)
    "\u001b[47m "
    "\u001b[40m "))

(defn get-pixel-row [row-vals]
  (apply str (map get-pixel row-vals)))

(defn part-one []
  (->>
   (get-encoded-image)
   (partition image-size)
   (map frequencies)
   (apply min-key #(get % 0))
   ((fn [freqs] (* (get freqs 1) (get freqs 2))))))

(defn part-two []
  (->>
   (get-encoded-image)
   (partition image-size)
   (apply (partial map get-color))
   (partition image-width)
   (map get-pixel-row)
   (clojure.string/join "\n")))

(defn -main []
  (println (str "Part 1:" (part-one)))
  (println "Part 2:")
  (println (str (part-two) "\u001b[0m")))