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

(defn starting-point [graph]
  (let [edges (set (reduce concat '() (vals graph)))
        nodes (set (reduce conj '() (keys graph)))]
    (first (clojure.set/difference nodes edges))))

(defn req-parents [graph edge]
  (reduce (fn [s [k edges]]
            (if ((set edges) edge)
              (conj s k)
              s)) #{} graph))

(defn solve-1 [graph]
  (let [start (starting-point graph)]
    (loop [node start ;; The current node
           candidates #{} ;; The candidates to select from
           result (list start)]
      (let [edges (get graph node)
            candidates (set (concat edges candidates))
            selected (first (filter (fn [c]
                                      ;; Only select candidate if all of its prereqs is in result
                                      (let [reqs (req-parents graph c)]
                                        (clojure.set/subset? reqs (set result))))
                                    (sort candidates)))]
        (if selected
          (recur selected (clojure.set/difference candidates #{selected}) (conj result selected))
          (apply str (reverse result)))))))



