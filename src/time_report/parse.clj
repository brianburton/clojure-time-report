(ns time-report.parse
  (:require [time-report.core :as core]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [malli.core :as m]
            [malli.generator :as mg]))

; 0000-2359
(def hhmm-regex #"(([01][0-9])|(2[0-3]))[0-5][0-9]")

; A single time span in the source file.
; :start and :stop are strings of hhmm
; :elapsed is in minutes
(def time-span-schema
  [:map
   [:start hhmm-regex]
   [:stop hhmm-regex]
   [:elapsed [:int {:min 1 :max 900}]]])

; Group of time spans associated with a client/project.
; :elapsed is sum of :elapsed from the time spans.
(def time-map-schema
  [:map
   [:client #"[-a-z]+"]
   [:project #"[- a-zA-Z]+"]
   [:elapsed [:int {:min 1 :max 1440}]]
   [:times [:vector time-span-schema]]])

; A single date and all of its associated time-maps.
; process-file passes a sequence of these to its processor-fn argument.
(def date-map-schema
  [:map
   [:year [:int {:min 2000 :max 2200}]]
   [:month [:int {:min 1 :max 12}]]
   [:day [:int {:min 1 :max 31}]]
   [:day-number [:int {:min 0 :max 6}]]
   [:week-number [:int {:min 0 :max 10000}]]
   [:times [:vector time-map-schema]]])

(comment
  (mg/generate time-span-schema)
  (mg/generate time-map-schema)
  (mg/generate date-map-schema))

(defn parse-date-line
  "Parse a line like 'Date: Tuesday 01/02/2024' into a map with broken out date components."
  [line]
  (if-some [[_ s] (re-matches #"^Date: \w+ (\d\d/\d\d/\d\d\d\d)$" line)]
    (let [d (core/parse-date s)]
      {:month (.getMonthValue d)
       :day (.getDayOfMonth d)
       :year (.getYear d)
       :day-number (core/day-of-week d)
       :week-number (core/week-number d)})
    (throw (ex-info "invalid date line" {:type :invalid-date-line :text line}))))

(defn parse-time-strings
  "Parse a pair of strings like ['0900' '1005'] into a map containing the parsed times and elapsed minutes."
  [[start stop]]
  (when (not-empty stop)
    (let [s (core/parse-time start)
          e (core/parse-time stop)]
      {:start start
       :stop stop
       :elapsed (core/elapsed s e)})))

(defn parse-times-group
  "Parse a string containing comma separated time spans like '0900-1105,1215-1725' into
   a vector of maps representing each of the time spans."
  [group]
  (let [spans (str/split group #",")
        splits (map #(str/split % #"-") spans)]
    (filter some? (map parse-time-strings splits))))

(defn assert-times-distinct
  [times-vec time-line]
  (let [sorted (sort-by :start times-vec)
        all-times (flatten (map #(core/select-values % [:start :stop]) sorted))]
    (doseq [[prev curr] (core/consecutive-items-seq "0000" all-times)]
      (when (> (compare prev curr) 0)
        (throw (ex-info "invalid time sequence" {:type :time-sequence-overlap :text time-line :prev prev :current curr}))))))

(defn parse-time-line
  "Parse a string like 'client,project: 0900-1105,1215-1720' into a map containing keys for the client and
   project strings, a vector of the time spans, and the total elapsed minutes for all time spans."
  [incomplete-ok time-line]
  (let
   [regex (if incomplete-ok
            #"^([-a-z]+),([- a-zA-Z]+): ((\d\d\d\d-)|((\d\d\d\d-\d\d\d\d)(,(\d\d\d\d-\d\d\d\d))*(,\d\d\d\d-)?))$"
            #"^([-a-z]+),([- a-zA-Z]+): ((\d\d\d\d-\d\d\d\d)(,(\d\d\d\d-\d\d\d\d))*)$")
    matched (re-matches regex time-line)]
    (if-some [[_ client project times] matched]
      (let [times-vec (vec (parse-times-group times))
            elapsed (apply + (map :elapsed times-vec))]
        (assert-times-distinct times-vec time-line)
        {:client client
         :project project
         :times times-vec
         :elapsed elapsed})
      (throw (ex-info "invalid time line" {:type :invalid-time-line :text time-line})))))

(defn normalize-line
  "Remove comments and whitespace.  Returns nil if the line contained only a comment."
  [raw-string]
  (let [comment-start (.indexOf raw-string "--")]
    (if (>= comment-start 0)
      (let [normalized (.strip (.substring raw-string 0 comment-start))]
        (if (empty? normalized) nil normalized))
      (.strip raw-string))))

(defn bounded-line-seq
  "Lazily returns lines from the seq until out of lines or a line equals 'END'."
  [lines-seq]
  (lazy-seq
   (when-not (empty? lines-seq)
     (let [line (first lines-seq)]
       (when-not (= "END" line)
         (cons line (bounded-line-seq (rest lines-seq))))))))

(defn calc-file-blocks
  "Partition a sequence of lines into a sequence of vectors of adjacent non-blank lines."
  [lines-seq]
  (filter (fn [v] (not-empty (first v)))
          (->> lines-seq
               (map normalize-line)
               (filter some?)
               (bounded-line-seq)
               (partition-by empty?)
               (map vec))))

(defn parse-file-block
  "Parse a block of adjacent non-blank lines from a file.
   Expects the first line to contain 'Date: day-name MM/dd/yyyy' and remaaining lines
   to contain 'client,project: hhmm-hhmm,hhmm-hhmm...'.
   Produces a seq of maps containing one one day of data per map."
  [today-date-map [date-line & time-lines]]
  (let [date-line-map (parse-date-line date-line)
        incomplete-time-span-ok (core/same-date? today-date-map date-line-map)
        time-lines-vec (filter some? (map (partial parse-time-line incomplete-time-span-ok) time-lines))
        all-times (flatten (map :times time-lines-vec))]
    (when-not (nil? date-line-map)
      (assert-times-distinct all-times date-line)
      (assoc date-line-map :times (vec time-lines-vec)))))

(defn process-file
  "Read file as a lazy sequence of lines and pass it to the provided processing function
   to compute a result."
  [file-name processor-fn]
  (with-open [rdr (io/reader file-name)]
    (doall (processor-fn (line-seq rdr)))))

(defn assert-dates-sorted
  [dms]
  (map (fn [[prev-dm curr-dm]]
         (let [prev-date (core/select-values prev-dm [:year :month :day])
               curr-date (core/select-values curr-dm [:year :month :day])]
           (if (< 0 (compare curr-date prev-date))
             curr-dm
             (throw (ex-info "Dates are not sorted." {:type :unsorted-dates :prev prev-date :current curr-date})))))
       (core/consecutive-items-seq {:year 0 :month 0 :day 0} dms)))

(defn parse-file
  [file-name today-date filter-fn]
  (process-file file-name
                (fn [lines-seq]
                  (->> lines-seq
                       calc-file-blocks
                       (map (partial parse-file-block today-date))
                       assert-dates-sorted
                       (filter filter-fn)))))
