(ns advc.day5
  (:require [advc.core :as core]))

(defn char-diff [c c'] (Math/abs (- (int c) (int c'))))
(defn reacts? [c c']
  (if (nil? c')
    false
    (= (char-diff c c') 32)))

(defn react-once [polymer]
  (loop [result []
         input polymer]
    (let [[a & b] (take 2 input)]
      (if a
       (if (reacts? a (first b))
         (recur result (drop 2 input))
         (recur (conj result a) (drop 1 input)))
       result))))

(defn react [polymer]
  (loop [input polymer]
    (let [reacted (react-once input)]
      (if (= (count reacted) (count input))
        reacted
        (recur reacted)))))

(def test-input "dabAcCaCBAcCcaDA")
(def puzzle-input (first (core/data-lines "day5")))

(defn solve-1 [polymer]
  (count (react polymer)))
