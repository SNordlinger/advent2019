(ns comp (:require [clojure.math.combinatorics :as combo]
                   [clojure.core.async :as async :refer [<! >! go]]))

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
    (try
      (loop [program initial-program start 0]
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
            99 nil
            (throw (Exception. (str "Bad Opcode: " opcode))))))
      (finally (async/close! out-chan)))))

(defn run-sync [program input]
  (let [in-chan (async/to-chan input) out-chan (async/chan)]
    (run program in-chan out-chan)
    (first (async/alts!! [(async/into [] out-chan)
                          (async/timeout 10000)]))))

(defn run-program [in-chan out-chan]
  (->
   (get-program)
   (run in-chan out-chan)))

(defn make-input-chan [phase]
  (let [ch (async/chan)]
    (async/put! ch phase)
    ch))

(defn amplify-signal [program phase-settings]
  (let [out-chans (repeatedly (count phase-settings) async/chan)
        multi-chan (async/mult (last out-chans))
        in-chans (cons (async/chan 10) (drop-last out-chans))
        final-chan (async/chan)]
    (async/tap multi-chan (first in-chans))
    (async/tap multi-chan final-chan)
    (dorun (map (fn [in-chan out-chan] (run program in-chan out-chan))
                in-chans
                out-chans))
    (dorun (map async/put! in-chans phase-settings))
    (async/put! (first in-chans) 0)
    (last (first (async/alts!! [(async/into [] final-chan)
                                (async/timeout 10000)])))))

(defn get-max-signal [program possible-phases]
  (apply max (map (partial amplify-signal program)
                  (combo/permutations possible-phases))))

(defn part-one []
  (get-max-signal (get-program) [0 1 2 3 4]))

(defn part-two []
  (get-max-signal (get-program) [5 6 7 8 9]))