(ns time-report.mode.random
  (:require [clojure.string :as str]
            [time-report.core :as core]
            [time-report.print :as pr]))

(def projects ["nasa,navigation system"
               "nasa,saturn v launch"
               "nasa,astronaut recovery"
               "nasa,monkey training"
               "nasa,meeting"
               "spacex,elon meeting"
               "spacex,landing software"
               "spacex,navigation"
               "spacex,pr meeting",
               "blue,jeff meeting"
               "blue,aws interop"
               "blue,navigation fixes"
               "carnival,gps upgrade"
               "carnival,hull scrub"
               "carnival,lifeboat repairs"
               "carnival,band auditions"])

(defn- random-project [] (rand-nth projects))

(defn- random-projects [number]
  (reduce (fn [acc _] (conj acc (random-project)))
          #{}
          (range 0 (inc (rand-int number)))))

(defn- random-time []
  (format "%02d%02d"
          (if (< 5 (rand-int 10))
            (+ 8 (rand-int 4))
            (+ 13 (rand-int 4)))
          (rand-int 60)))

(defn- random-times []
  (reduce
   (fn [spans _] (conj spans (random-time)))
   ["0800" "1200" "1300" "1700"]
   (range 0 (inc (rand-int 6)))))

(defn- random-spans []
  (->> (random-times)
       (sort)
       (partition 2 1)
       (map (fn [[start end]] (format "%s-%s" start end)))
       (filter #(not= "1200-1300" %))))

(defn- time-line [spans]
  (str ((first spans) :project)
       ": "
       (str/join "," (map :span spans))))

(defn- random-time-lines []
  (let [projects (vec (random-projects (inc (rand-int 4))))
        spans (random-spans)
        assignments (map (fn [span] {:project (rand-nth projects) :span span}) spans)
        grouped (group-by :project assignments)]
    (map time-line (vals grouped))))

(defn- dates []
  (let [today (core/current-date)]
    (map #(core/prior-date today (abs %)) (range -45 1))))

(defn- date-line [d]
  (let [{year :year month :month day :day} (core/date-to-map d)]
    (format "Date: %s %02d/%02d/%04d" (core/day-name d) month day year)))

(defn- date-and-time-lines [d]
  (if (< (rand-int 7) 5)
    (concat [(date-line d)] (random-time-lines) [""])
    []))

(defn random-data-file []
  (flatten (map date-and-time-lines (dates))))

(defn execute
  []
  (pr/print-lines (random-data-file)))