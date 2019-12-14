(ns day-11.comp)

(defn get-program
  []
  (->>
   (slurp "../input.txt")
   clojure.string/trim-newline
   (#(clojure.string/split % #","))
   (map #(Long/parseLong %))
   vec))

(defn apply-param-mode
  [start-index rel-base mode args]
  (map-indexed (fn [ind arg]
                 (case (mod (quot mode (apply * (repeat ind 10))) 10)
                   0 arg
                   1 (+ start-index ind)
                   2 (+ arg rel-base)
                   (throw (Exception. "Bad parameter mode"))))
               args))

(defn expand-data
  [program-data & args]
  (let [tape (program-data :tape)
        over (- (apply max args) (count tape))]
    (if (> over 0)
      (assoc program-data :tape (into tape (repeat (+ 1 over) 0)))
      program-data)))

(defn apply-op
  [program-data op [in-1 in-2 out-addr]]
  (let [expanded-data (expand-data program-data in-1 in-2 out-addr)
        tape (expanded-data :tape)
        current (expanded-data :current)
        [val-1 val-2] (map tape [in-1 in-2])
        new-tape (assoc tape out-addr (op val-1 val-2))]
    (assoc expanded-data
           :tape new-tape
           :current (+ current 4))))

(defn apply-input
  [program-data [addr]]
  (let [expanded-data (expand-data program-data addr)
        tape (expanded-data :tape)
        current (expanded-data :current)
        input (first (expanded-data :input))
        new-tape (assoc tape addr input)]
    (assoc expanded-data
           :tape new-tape
           :input (rest (expanded-data :input))
           :current (+ current 2))))

(defn add-output
  [program-data [addr]]
  (let [expanded-data (expand-data program-data addr)
        tape (expanded-data :tape)
        current (expanded-data :current)
        output (expanded-data :output)
        val (tape addr)]
    (assoc expanded-data
           :current (+ current 2)
           :output (conj output val))))

(defn jump-on-cond
  [program-data condition [val addr]]
  (let [expanded-data (expand-data program-data val addr)
        tape (expanded-data :tape)
        current (expanded-data :current)
        [val-1 addr] (map tape [val addr])]
    (if (condition val-1)
      (assoc expanded-data :current addr)
      (assoc expanded-data :current (+ current 3)))))

(defn adjust-rel-base
  [program-data [addr]]
  (let [expanded-data (expand-data program-data addr)
        tape (expanded-data :tape)
        current (expanded-data :current)
        rel-base (expanded-data :rel-base)
        val (tape addr)]
    (assoc expanded-data
           :rel-base (+ rel-base val)
           :current (+ current 2))))

(defn run-cycle
  [program-data]
  (let [tape (program-data :tape)
        current (program-data :current)
        rel-base (program-data :rel-base)
        [first-val & raw-args] (subvec tape (program-data :current))
        opcode (mod first-val 100)
        param-mode (quot first-val 100)
        args (apply-param-mode (+ current 1) rel-base param-mode raw-args)]
    (case opcode
      1 (apply-op program-data + args)
      2 (apply-op program-data * args)
      3 (apply-input program-data args)
      4 (add-output program-data args)
      5 (jump-on-cond program-data (partial not= 0) args)
      6 (jump-on-cond program-data (partial = 0) args)
      7 (apply-op program-data (fn [val-1 val-2] (if (< val-1 val-2) 1 0)) args)
      8 (apply-op program-data (fn [val-1 val-2] (if (= val-1 val-2) 1 0)) args)
      9 (adjust-rel-base program-data args)
      99 (assoc program-data :exited? true)
      (throw (Exception. (str "Bad Opcode: " opcode))))))


(defn initial-state
  ([tape]
   initial-state [tape []])
  ([tape input]
   {:tape tape
    :input input
    :output []
    :current 0
    :exited? false
    :rel-base 0}))

(defn run
  ([tape]
   (run tape []))
  ([tape input]
   (let [program-data (initial-state tape input)]
     (first (filter :exited?
                    (iterate run-cycle program-data))))))

(defn run-program
  [input]
  (->
   (get-program)
   (run input)
   (:output)))

(defn -main []
  (println (str "Part 1: " (run-program [1])))
  (println (str "Part 2: " (run-program [2]))))