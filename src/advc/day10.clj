(ns advc.day10
  (:require [advc.core :as core]
            [clojure.string :as str]))

(def test-lines
  (map str/trim (str/split-lines
                 "position=< 9,  1> velocity=< 0,  2>
                  position=< 7,  0> velocity=<-1,  0>
                  position=< 3, -2> velocity=<-1,  1>
                  position=< 6, 10> velocity=<-2, -1>
                  position=< 2, -4> velocity=< 2,  2>
                  position=<-6, 10> velocity=< 2, -2>
                  position=< 1,  8> velocity=< 1, -1>
                  position=< 1,  7> velocity=< 1,  0>
                  position=<-3, 11> velocity=< 1, -2>
                  position=< 7,  6> velocity=<-1, -1>
                  position=<-2,  3> velocity=< 1,  0>
                  position=<-4,  3> velocity=< 2,  0>
                  position=<10, -3> velocity=<-1,  1>
                  position=< 5, 11> velocity=< 1, -2>
                  position=< 4,  7> velocity=< 0, -1>
                  position=< 8, -2> velocity=< 0,  1>
                  position=<15,  0> velocity=<-2,  0>
                  position=< 1,  6> velocity=< 1,  0>
                  position=< 8,  9> velocity=< 0, -1>
                  position=< 3,  3> velocity=<-1,  1>
                  position=< 0,  5> velocity=< 0, -1>
                  position=<-2,  2> velocity=< 2,  0>
                  position=< 5, -2> velocity=< 1,  2>
                  position=< 1,  4> velocity=< 2,  1>
                  position=<-2,  7> velocity=< 2, -2>
                  position=< 3,  6> velocity=<-1, -1>
                  position=< 5,  0> velocity=< 1,  0>
                  position=<-6,  0> velocity=< 2,  0>
                  position=< 5,  9> velocity=< 1, -2>
                  position=<14,  7> velocity=<-2,  0>
                  position=<-3,  6> velocity=< 2, -1>")))

(defn line-parser []
  (let [re #"position=<\s*(.*),\s*(.*)>\s*velocity=<\s*(.*),\s*(.*)>"]
    (fn [line]
      (if-let [[x y dx dy] (map #(Integer/parseInt %) (rest (re-find re line)))]
        [[x y] [dx dy]]
        (throw (ex-info "Cannot parse" {:input line}))))))

(def test-input (map (line-parser) test-lines))
(def puzzle-input (map (line-parser) (core/data-lines "day10")))

(defn step [state]
  (doall (map (fn [[pos dir]] [(doall (map + pos dir)) dir]) state)))

(defn bounds [state]
  (let [[min-x max-x min-y max-y]
        (doall (reduce (fn [[min-x max-x min-y max-y] [[x y] _]]
                   [(if (< x min-x) x min-x)
                    (if (> x max-x) x max-x)
                    (if (< y min-y) y min-y)
                    (if (> y max-y) y max-y)])
                 [Integer/MAX_VALUE
                  Integer/MIN_VALUE
                  Integer/MAX_VALUE
                  Integer/MIN_VALUE]
                 state))]
    [[min-x max-x] [min-y max-y]]))

(defn dimension [state]
  (let [[[min-x max-x] [min-y max-y]] (bounds state)]
    [(- max-x min-x) (- max-y min-y)]))

(defn draw [state]
  (let [[[min-x max-x] [min-y max-y]] (bounds state)
        position? (set (map first state))]
    (doseq [y (range min-y (inc max-y))]
      (println (map (fn [x]
                      (if (position? [x y]) "x" "."))
                    (range min-x (inc max-x)))))))

(defn find-smallest-iteration [state]
  (loop [idx 0
         state state
         [dx dy :as dim] (dimension state)]
    (let [next-state (step state)
          [ndx ndy :as next-dim] (dimension next-state)]
      (if (and (> dx ndx) (> dy ndy))
        (recur (inc idx) next-state next-dim)
        (do (println "Found smallest area at" idx ". Dimensions:" dim)
            (draw state)
            state)))))


