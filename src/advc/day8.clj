(ns advc.day8
  (:require [advc.core :as util]
            [clojure.string :as str]))


(defn parse-line [line] (map #(Integer/parseInt %) (str/split line #"\s+")))

(def test-input (parse-line "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))
(def puzzle-input (parse-line (first (util/data-lines "day8"))))








