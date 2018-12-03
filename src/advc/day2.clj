(ns advc.day2
  (:require [advc.core :refer :all]))

(def test-input (data-lines "day2"))

(def sample-input ["abcdef"
                   "bababc"
                   "abbcde"
                   "abcccd"
                   "aabcdd"
                   "abcdee"
                   "ababab"])

(defn repeated-chars [word]
  (set (vals (frequencies word))))

(defn checksum [words]
  (let [word-sets   (map repeated-chars words)
        two-words   (count (filter #(% 2) word-sets))
        three-words (count (filter #(% 3) word-sets))]
    (* two-words three-words)))

(defn solve-1 []
  (checksum test-input))

(defn box-match? [a b]
  (and (= (count a) (count b))
       (= (dec (count a)) (count (filter #(apply = %) (map vector a b))))))
