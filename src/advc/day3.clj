(ns advc.day3
  (:require [advc.core :refer :all]
            [clojure.math.combinatorics :as combo]))

(defn coords [{:keys [x y width height]}]
  (set (for [x' (take width (iterate inc x))
             y' (take height (iterate inc y))]
         [x' y'])))

(def remapper (comp
               (fn [m] (assoc m :coords (coords m)))
               (fn [m] (map-vals #(Integer/parseInt %2) m))
               (partial re-map
                        #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)"
                        [:id :x :y :width :height])))

(def puzzle-input
  (->> (data-lines "day3")
       (map remapper)))

(def test-data (map remapper ["#1 @ 1,3: 4x4"
                              "#2 @ 3,1: 4x4"
                              "#3 @ 5,5: 2x2"]))

(defn overlapping-coords
  [{c :coords :as m} {c' :coords :as m'}]
  (clojure.set/intersection c c'))

(defn solve-1a [areas]
  (let [processed (reduce (fn [s {:keys [coords]}]
                            (let [freqs (frequencies coords)]
                              (merge-with (fn [v _] (inc v)) s freqs)))
                          {} areas)]
    (filter (fn [[_ v]] (> v 1)) processed)))

(defn solve-2 [areas]
  (let [overlapped (keys (solve-1a areas))]
    (filter (fn [{:keys [coords] :as m}]
                      (empty? (clojure.set/intersection coords (set overlapped))))
                    areas)))
