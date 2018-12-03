(ns advc.day1
  (:require [advc.core :refer :all]))

(defn calc-frequency [freqs] (reduce + 0 freqs))

(defn solve-1 []
  (->> (data-lines "day1")
      (map #(Integer/parseInt %))
      (calc-frequency)))

(defn repeated-frequency [freqs]
  (let [vals (reductions + 0 (cycle freqs))
        pred (let [seen (atom #{})]
               (fn [v] (if (@seen v)
                         true
                         (do (swap! seen conj v)
                             false))))]
    (first (filter pred vals))))

(defn solve-2 []
  (->> (data-lines "day1")
       (map #(Integer/parseInt %))
       (repeated-frequency)))



