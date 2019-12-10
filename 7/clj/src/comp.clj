(ns comp (:require [clojure.math.combinatorics :as combo]
                   [clojure.core.async :as async :refer [<! >! to-chan chan go alts!! timeout close!]]))

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

(defn get-output [program args param-mode]
  (let [[val] (apply-param-mode program param-mode args)]
    val))

(defn jump-on-cond [program start cond args param-mode]
  (let [[val-1 addr] (apply-param-mode program param-mode args)]
    (if (cond val-1)
      addr
      (+ start 3))))

(defn run
  [initial-program in-chan out-chan]
  (go
    (loop [program initial-program start 0]
      (if (= (get program start) 99)
        (close! out-chan)
        (let [[first-val & args] (subvec program start)
              opcode (mod first-val 100)
              param-mode (quot first-val 100)]
          (case opcode
            1 (recur (apply-op program + args param-mode)
                     (+ start 4))
            2 (recur (apply-op program * args param-mode)
                     (+ start 4))
            3 (recur (assoc program (first args) (<! in-chan))
                     (+ start 2))
            4 (do (>! out-chan (get-output program args param-mode))
                  (recur program
                         (+ start 2)))
            5 (recur program
                     (jump-on-cond program start (partial not= 0) args param-mode))
            6 (recur program
                     (jump-on-cond program start (partial = 0) args param-mode))
            7 (recur (apply-op program (fn [val-1 val-2] (if (< val-1 val-2) 1 0)) args param-mode)
                     (+ start 4))
            8 (recur (apply-op program (fn [val-1 val-2] (if (= val-1 val-2) 1 0)) args param-mode)
                     (+ start 4))
            (throw (Exception. (str "Bad Opcode: " opcode)))))))))

(defn run-sync [program input]
  (let [in-chan (to-chan input) out-chan (chan)]
    (run program in-chan out-chan)
    (first (alts!! [(async/into [] out-chan)
                    (timeout 10000)]))))

(defn run-program [in-chan out-chan]
  (->
   (get-program)
   (run in-chan out-chan)))

(defn apply-amp [program phase input]
  (first (run-sync program [phase input])))

(defn amplify-signal [program phase-settings]
  ((apply comp (map (fn [phase] (partial apply-amp program phase))
                    (reverse phase-settings)))
   0))

(defn get-max-signal [program]
  (apply max (map (partial amplify-signal program)
                  (combo/permutations [0 1 2 3 4]))))

(defn part-one []
  (get-max-signal (get-program)))