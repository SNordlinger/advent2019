(ns comp (:require [clojure.math.combinatorics :as combo]))

(defn get-program []
  (->>
   (slurp "../input.txt")
   clojure.string/trim-newline
   (#(clojure.string/split % #","))
   (map #(Integer/parseInt %))
   vec))

(defn apply-op [program op [in-addr-1 in-addr-2 out-addr]]
  (assoc program out-addr (op (get program in-addr-1)
                              (get program in-addr-2))))

(defn run [initial-program]
  (loop [program initial-program start 0]
    (if (= (get program start) 99)
      program
      (let [end (+ start 4)
            inst (subvec program start end)
            [opcode & addrs] inst]
        (case opcode
          1 (recur (apply-op program + addrs) end)
          2 (recur (apply-op program * addrs) end)
          (throw (Exception. "Bad Opcode")))))))

(defn get-input-result [noun verb]
  (->
   (get-program)
   (assoc 1 noun)
   (assoc 2 verb)
   run
   first))

(defn find-noun-verb []
  (first (filter (fn [[noun verb]] (= (get-input-result noun verb)
                                      19690720))
                 (combo/cartesian-product (range 0 100) (range 0 100)))))

(defn -main []
  (println (str "Part 1: " (get-input-result 12 2)))
  (println (str "Part 2: " (vec (find-noun-verb)))))