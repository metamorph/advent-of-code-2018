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
  (println "Reacting")
  (loop [input polymer]
    (let [reacted (react-once input)]
      (if (= (count reacted) (count input))
        reacted
        (recur reacted)))))

(def test-input "dabAcCaCBAcCcaDA")
(def puzzle-input (first (core/data-lines "day5")))

(defn solve-1 [polymer]
  (count (react polymer)))

(def all-chars
  (map char (range (int \a) (inc (int \z)))))

(defn same-char? [c c']
  (let [d (char-diff c c')]
    (or (= d 0) (= d 32))))


(defn filtered-polymer [polymer c]
  (filter (complement (partial same-char? c)) polymer))

;; SLOW!
(defn solve-2 [polymer]
  (let [reduced (react polymer)]
   (count (->> (map (fn [c] [c (react (filtered-polymer reduced c))])
                    all-chars)
               (sort-by #(count (last %)))
               first
               last))))


