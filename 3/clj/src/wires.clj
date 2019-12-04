(ns wires)

(defn inst-path [start move axis]
  (let [distance (Math/abs move)
        op (if (pos? move) + -)]
    (map (fn [d] (assoc start axis (op (start axis) d)))
         (range 1 (+ distance 1)))))

(defn apply-inst [start inst]
  (println start inst)
  (let [dir (first inst)
        distance (Integer/parseInt (subs inst 1 2))]
    (case dir
      \R (inst-path start distance :x)
      \L (inst-path start (- distance) :x)
      \U (inst-path start distance :y)
      \D (inst-path start (- distance) :y))))

(def instructions ["R8" "U5" "L5" "D3"])

(apply-inst {:x 0 :y 0} "L5")

(defn get-path [start instructions]
  (reduce (fn [path inst] (concat path (apply-inst (last path) inst)))
          [start]
          instructions))