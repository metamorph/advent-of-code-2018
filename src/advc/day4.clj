(ns advc.day4
  (:require [advc.core :refer :all]))

(def test-lines
  ["[1518-11-01 00:00] Guard #10 begins shift"
  "[1518-11-01 00:05] falls asleep"
  "[1518-11-01 00:25] wakes up"
  "[1518-11-01 00:30] falls asleep"
  "[1518-11-01 00:55] wakes up"
  "[1518-11-01 23:58] Guard #99 begins shift"
  "[1518-11-02 00:40] falls asleep"
  "[1518-11-02 00:50] wakes up"
  "[1518-11-03 00:05] Guard #10 begins shift"
  "[1518-11-03 00:24] falls asleep"
  "[1518-11-03 00:29] wakes up"
  "[1518-11-04 00:02] Guard #99 begins shift"
  "[1518-11-04 00:36] falls asleep"
  "[1518-11-04 00:46] wakes up"
  "[1518-11-05 00:03] Guard #99 begins shift"
  "[1518-11-05 00:45] falls asleep"
  "[1518-11-05 00:55] wakes up"])


(defn parse-event [s]
  (condp re-find s
    #"falls asleep" {:type :asleep}
    #"wakes up" {:type :wakeup}
    #"Guard #(\d*)\s.*" :>> (fn [[_ id]] {:type :start :guard id})))

(defn parse-line [line]
  (re-map2 #"\[\d\d\d\d-(\d\d-\d\d)\s\d\d:(\d\d)\]\s(.*)" [[:date identity]
                                                     [:minute #(Integer/parseInt %)]
                                                     [:event parse-event]] line))
(defn with-guard-attr [events]
  (let [current (atom nil)]
    (map (fn [{{:keys [guard]} :event :as e}]
           (assoc-in e [:event :guard] (if guard (reset! current guard) @current)))
         events)))

(defn parse-input [lines]
  (with-guard-attr
    (->> (map parse-line lines)
         (sort-by (fn [{:keys [date minute]}] [date minute])))))

;; [:asleep :wakeup] [:asleep :start] [:asleep :asleep]

(defn minutes [guard start end]
  (for [m (range start end)] [guard m]))


(defn sleep-cycles [events]
  (let [pairs (partition-all 2 1 events)]
    (mapcat (fn [[{:as e
                   m :minute
                   {t :type g :guard} :event}
                  {:as e'
                   m' :minute
                   {t' :type} :event}]]
              (case [t t']
                [:asleep :wakeup] (minutes g m m')
                [:asleep :start] (minutes g m m')
                [:asleep :asleep] (minutes g m m')
                []))
            pairs)))

(defn solve-1 [events]
  (let [sleeping-at (reduce (fn [r [g m]] (update r g conj m)) {} (sleep-cycles events))
        [sleepy-guard minutes :as e] (first (sort-by (fn [[g entries]] (* -1 (count entries))) sleeping-at))
        [sleepy-minute _] (apply max-key val (frequencies minutes))]
    (* (Integer/parseInt sleepy-guard) sleepy-minute)))

(defn solve-2 [events]
  (let [sleeping-at (reduce (fn [r [g m]] (update r g conj m)) {} (sleep-cycles events))
        sleep-freq (map (fn [[g ms]] [g (frequencies ms)]) sleeping-at)
        [guard freqs] (first (sort-by (fn [[_ freqs]] (* -1 (last (apply max-key val freqs)))) sleep-freq))]

    (* (Integer/parseInt guard) (first (apply max-key val freqs)))))

(def puzzle-input (parse-input (data-lines "day4")))
(def test-input (parse-input test-lines))

