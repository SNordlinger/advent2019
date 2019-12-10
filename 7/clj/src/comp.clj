(ns comp (:require [clojure.math.combinatorics :as combo]))

(defn get-program []
  (->>
   (slurp "../input.txt")
   clojure.string/trim-newline
   (#(clojure.string/split % #","))
   (map #(Integer/parseInt %))
   vec))

(defn apply-param-mode [program mode args]
  (map (fn [arg place]
         (case (mod (quot mode place) 10)
           0 (program arg)
           1 arg
           (throw (Exception. "Bad parameter mode"))))
       args
       (iterate (partial * 10) 1)))

(defn apply-op [program op [in-1 in-2 out-addr] param-mode]
  (let [[val-1 val-2] (apply-param-mode program param-mode [in-1 in-2])]
    (assoc program out-addr (op val-1 val-2))))

(defn add-output [program args param-mode output]
  (let [[val] (apply-param-mode program param-mode args)]
    (conj output val)))

(defn jump-on-cond [program start cond args param-mode]
  (let [[val-1 addr] (apply-param-mode program param-mode args)]
    (if (cond val-1)
      addr
      (+ start 3))))

(defn run
  ([initial-program]
   (run initial-program []))
  ([initial-program initial-input]
   (loop [program initial-program start 0 input initial-input output []]
     (if (= (get program start) 99)
       output
       (let [[first-val & args] (subvec program start)
             opcode (mod first-val 100)
             param-mode (quot first-val 100)]
         (case opcode
           1 (recur (apply-op program + args param-mode)
                    (+ start 4)
                    input
                    output)
           2 (recur (apply-op program * args param-mode)
                    (+ start 4)
                    input
                    output)
           3 (recur (assoc program (first args) (first input))
                    (+ start 2)
                    (rest input)
                    output)
           4 (recur program
                    (+ start 2)
                    input
                    (add-output program args param-mode output))
           5 (recur program
                    (jump-on-cond program start (partial not= 0) args param-mode)
                    input
                    output)
           6 (recur program
                    (jump-on-cond program start (partial = 0) args param-mode)
                    input
                    output)
           7 (recur (apply-op program (fn [val-1 val-2] (if (< val-1 val-2) 1 0)) args param-mode)
                    (+ start 4)
                    input
                    output)
           8 (recur (apply-op program (fn [val-1 val-2] (if (= val-1 val-2) 1 0)) args param-mode)
                    (+ start 4)
                    input
                    output)
           (throw (Exception. (str "Bad Opcode: " opcode)))))))))

(defn run-program [input]
  (->
   (get-program)
   (run input)))

(defn apply-amp [program phase input]
  (first (run program [phase input])))

(defn amplify-signal [program phase-settings]
  ((apply comp (map (fn [phase] (partial apply-amp program phase))
                    (reverse phase-settings)))
   0))

(defn get-max-signal [program]
  (apply max (map (partial amplify-signal program)
                  (combo/permutations [0 1 2 3 4]))))

(defn part-one []
  (get-max-signal (get-program)))

(defn -main []
  (println (str "Part 1: " (run-program 1)))
  (println (str "Part 2: " (run-program 5))))