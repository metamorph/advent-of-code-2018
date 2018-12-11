(ns advc.day7
  (:require [advc.core :as core]))

;; Input

(def test-data
  ["Step C must be finished before step A can begin."
   "Step C must be finished before step F can begin."
   "Step A must be finished before step B can begin."
   "Step A must be finished before step D can begin."
   "Step B must be finished before step E can begin."
   "Step D must be finished before step E can begin."
   "Step F must be finished before step E can begin."])

(defn parse-stmt [stmt]
  (let [[_ & [from to]] (re-find #"Step\s(.)\s.*before step\s(.).*" stmt)]
    [from to]))

(defn parse-lines [lines]
  (reduce (fn [m [from to]] (update m from conj to))
          {} (map parse-stmt lines)))

(def test-input (parse-lines test-data))
(def puzzle-input (parse-lines (core/data-lines "day7")))
(def puzzle-input2 (parse-lines (core/data-lines "day71")))

(defn starting-points [graph]
  (let [edges (set (reduce concat '() (vals graph)))
        nodes (set (reduce conj '() (keys graph)))]
    (sort (clojure.set/difference nodes edges))))

(defn req-parents [graph edge]
  (reduce (fn [s [k edges]]
            (if ((set edges) edge)
              (conj s k)
              s)) #{} graph))

(defn requirements [graph]
  (reduce (fn [m [k edges]]
            (reduce (fn [m e]
                      (update m e conj k)) m edges))
          {} graph))

(defn solve-1 [graph]
  (let [starts (starting-points graph)
        start (first starts)
        reqs (requirements graph)]
    (loop [idx 0
           node start ;; The current node
           candidates (set starts) ;; The candidates to select from
           completed (list start)]
      (let [edges (get graph node)
            candidates (clojure.set/difference (set (concat edges candidates)) (set completed))
            selected (first (filter (fn [c]
                                      (let [r (set (get reqs c []))
                                            v (conj (set completed) node)
                                            pass? (clojure.set/subset? r v)]
                                       pass?))
                                    (sort candidates)))]
        (if selected
          (recur (inc idx) selected (clojure.set/difference candidates #{selected}) (conj completed selected))
          (apply str (reverse completed)))))))

(defn work-time [c] (inc (- (int (first c)) (int \A))))

(defn solve-2 [graph n-workers]
  (let [start-nodes (starting-points graph)])

