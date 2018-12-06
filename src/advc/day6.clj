(ns advc.day6
  (:require [advc.core :as core]
            [clojure.string :as str]))

(defn line->coord [line]
  (mapv #(Integer/parseInt %)
        (str/split line #"\s*,\s*")))

(defn m-dist [[x y] [x' y']]
  (+ (Math/abs (- x x'))
     (Math/abs (- y y'))))

(defn outer-area "Get the max area of all points in `coords`"
  [coords]
  (let [[max-x _] (apply max-key first coords)
        [_ max-y] (apply max-key last coords)]
    [[0 max-x] [0 max-y]]))

(defn closest-point
  "Return `[x y]` for the point in `points` that is closest to `coord`."
  [coord points]
  (if (some #(= % coord) points)
     nil ;; if coord is in points - skip it.
     (let [closest-points (->> (map (fn [[x y :as p]] [p (m-dist p coord)]) points)
                               (sort-by last)
                               (partition-by last)
                               (first))]
       (if (= (count closest-points) 1)
         (first closest-points)
         nil))))

(defn acc-distance [coord points]
  (reduce + 0 (map (partial m-dist coord) points)))

(defn acc-matrix [points]
  (let [[[min-x max-x] [min-y max-y]] (outer-area points)
        coords (for [x (range min-x (inc max-x)) y (range min-y (inc max-y))] [x y])]
    (map (fn [c] [c (acc-distance c points)]) coords)))

(defn closest-matrix [coords bounds]
  (let [[[min-x max-x] [min-y max-y]] bounds
         matrix (for [x (range min-x (inc max-x)) y (range min-y (inc max-y))] [x y])]
     (filter (fn [[[_] v]] (some? v))
             (map (fn [p] [p (closest-point p coords)]) matrix))))

(defn infinite-areas
  "Get the set of areas names with infinite areas. That is - on the boundary."
  [matrix [[min-x max-x] [min-y max-y]]]
  (set (map (fn [[_ [c _]]] c)
             (filter (fn [[[x y] _]] (or (= x min-x)
                                         (= x max-x)
                                         (= y min-y)
                                         (= y max-y)))
                     matrix))))

(defn solve-1 [coords]
  (let [[[min-x max-x] [min-y max-y] :as bounds] (outer-area coords)
        matrix (closest-matrix coords bounds)]
    ;; Find all areas that or "on" the boundary
    ;; Exclude those
    ;; Then find the area with the max number of entries
    (let [inifinites (infinite-areas matrix bounds)]
      (frequencies (map (fn [[[_] [c _]]] c) (filter (complement (fn [[[_] [c _]]] (inifinites c))) matrix))))))

;; Input

(def test-input
  [[1 1]
   [1 6]
   [8 3]
   [3 4]
   [5 5]
   [8 9]])

(def puzzle-input
  (->> (core/data-lines "day6")
       (map line->coord)))
