(ns advc.core
  "Some util functions shared between different exercises."
  (:require [clojure.java.io :as io]))

(defn data-lines
  "Returns all line of `base-name`."
  [base-name]
  (with-open [f (io/reader (io/resource (format "%s.data" (name base-name))))]
    (doall (line-seq f))))

(defn re-map
  "Transform a line matching a regex to a map.
  Useful when parsing lines according to a certain pattern:
  `(map (partial re-map my-regex [:foo :bar]) (data-lines \"file\"))`"
  [re keys line]
  (if-let [[_ & groups] (re-find re line)]
    (zipmap keys groups)
    {}))

(defn map-vals [f m]
  (reduce (fn [a [k v]] (assoc a k (f k v)))
          {}
          m))

(defn permutations
  "Create pairs of permutations of elements in col."
  [col] col)
