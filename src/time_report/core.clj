(ns time-report.core
  (:gen-class)
  (:require [clojure.set :as set])
  (:require [java-time.api :as jt])
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:require [clojure.tools.cli :refer [parse-opts]]))

(defn my-flatten
  ([s] (my-flatten s ()))
  ([ss more]
   (lazy-seq
    (loop [s ss k more]
      (println s)
      (println k)
      (if (seqable? s)
        (if (empty? s)
          (if (empty? k) nil (recur (first k) (rest k)))
          (let [head (first s)
                tail (rest s)]
            (if (seqable? head)
              (recur head (cons tail k))
              (cons head (my-flatten tail k)))))
        (println (list? s) (empty? s) (seq? s) s))))))

(defn select-values
  "Create a vector containing value for each key (or nil if not in map)."
  [source-map keys]
  (vec (map #(get source-map %) keys)))

(defn sub-vec
  [v indexes]
  (reduce (fn [newv i] (conj newv (v i))) [] indexes))

(defn last-2-seq
  "Creates a lazy seq that reads from the provided seq and returns
   pairs of previous value plus current value.  Initial value can be
   provided when calling (nil used if none provided)."
  ([the-seq] (last-2-seq nil the-seq))
  ([prev-value the-seq]
   (lazy-seq
    (when-not (empty? the-seq)
      (let [this-value (first the-seq)]
        (cons [prev-value this-value] (last-2-seq this-value (rest the-seq))))))))

(defn parse-time [t]
  (try (jt/local-time "HHmm" t)
       (catch clojure.lang.ExceptionInfo _
         (throw (ex-info "invalid time string" {:type :invalid-time-string :text t})))))

(defn parse-date [d]
  (try (jt/local-date "MM/dd/yyyy" d)
       (catch clojure.lang.ExceptionInfo _
         (throw (ex-info "invalid date string" {:type :invalid-date-string :text d})))))

(defn elapsed [time1 time2] (.toMinutes (jt/duration time1 time2)))

(def ^:const empty-elapsed-vec [0 0 0 0 0 0 0])

; saturday first order:
; (def ^:const day-names ["SAT" "SUN" "MON" "TUE" "WED" "THU" "FRI"])
; (defn day-number [day] (let [d (.. day getDayOfWeek getValue)] (if (>= d 6) (- d 6) (+ d 1))))

; monday first order:
(def ^:const day-names ["MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN"])
(defn day-number [day] (let [d (.. day getDayOfWeek getValue)] (dec d)))

(defn day-index
  [day]
  (let [raw-base (jt/local-date 1996 1 1)
        real-base (jt/minus raw-base (jt/days (day-number raw-base)))]
    (jt/time-between :days real-base day)))

(defn week-number [day] (quot (day-index day) 7))

(defn current-date [] (jt/local-date))

(defn date-to-map [d]
  {:year (.getYear d)
   :month (.getMonthValue d)
   :day (.getDayOfMonth d)})

(defn leap-year? [year]
  (and (= 0 (mod year 4))
       (or (not= 0 (mod year 100))
           (= 0 (mod year 400)))))

(def ^:const months-with-31-days #{1 3 5 7 8 10 12})

(defn days-in-month [year month]
  (cond
    (contains? months-with-31-days month) 31
    (= 2 month) (if (leap-year? year) 29 28)
    :else 30))

(defn cycle-days
  [year month day]
  (if (< day 16)
    (set (range 1 16))
    (set (range 16 (+ 1 (days-in-month year month))))))

(defn same-cycle?
  [year month day]
  (let [good-days (cycle-days year month day)]
    (fn [{y :year m :month d :day}]
      (and
       (= year y)
       (= month m)
       (good-days d)))))

(defn current-cycle?
  [today]
  (let [current-year (.getYear today)
        current-month (.getMonthValue today)
        current-day (.getDayOfMonth today)]
    (same-cycle? current-year current-month current-day)))

(defn prev-cycle?
  [today]
  (let [current-year (.getYear today)
        current-month (.getMonthValue today)
        current-day (.getDayOfMonth today)
        prev-day (if (< current-day 16) 16 1)
        prev-month (if (< current-day 16)
                     (if (= current-month 1) 12 (dec current-month))
                     current-month)
        prev-year (if (< current-day 16)
                    (if (= current-month 1) (dec current-year) current-year)
                    current-year)]
    (same-cycle? prev-year prev-month prev-day)))

(defn fill-missing-days [dms]
  (let [present-days (set (map :day dms))
        first-dm (first dms)
        year (:year first-dm)
        month (:month first-dm)
        day (:day first-dm)
        all-days (cycle-days year month day)
        missing-days (set/difference all-days present-days)
        missing-dms (map (fn [d]
                           (let [ld (jt/local-date year month d)
                                 dn (day-number ld)
                                 wn (week-number ld)]
                             {:year year :month month :day d :day-number dn :week-number wn :times []}))
                         missing-days)]
    (vec (sort-by :day (concat dms missing-dms)))))

(defn parse-date-line
  "Parse a line like 'Date: Tuesday 01/02/2024' into a map with broken out date components."
  [line]
  (if-some [[_ s] (re-matches #"^Date: \w+ (\d\d/\d\d/\d\d\d\d)$" line)]
    (let [d (parse-date s)]
      {:month (.getMonthValue d)
       :day (.getDayOfMonth d)
       :year (.getYear d)
       :day-number (day-number d)
       :week-number (week-number d)})
    (throw (ex-info "invalid date line" {:type :invalid-date-line :text line}))))

(defn parse-time-strings
  "Parse a pair of strings like ['0900' '1005'] into a map containing the parsed times and elapsed minutes."
  [[start stop]]
  (when (not-empty stop)
    (let [s (parse-time start)
          e (parse-time stop)]
      {:start start
       :stop stop
       :elapsed (elapsed s e)})))

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
        all-times (flatten (map #(select-values % [:start :stop]) sorted))]
    (doseq [[prev curr] (last-2-seq "0000" all-times)]
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

(defn same-date?
  [a b]
  (apply = (map #(select-values % [:year :month :day]) [a b])))

(defn parse-file-block
  "Parse a block of adjacent non-blank lines from a file.
   Expects the first line to contain 'Date: day-name MM/dd/yyyy' and remaaining lines
   to contain 'client,project: hhmm-hhmm,hhmm-hhmm...'.
   Produces a seq of maps containing one one day of data per map."
  [today-date-map [date-line & time-lines]]
  (let [date-line-map (parse-date-line date-line)
        incomplete-date-ok (same-date? today-date-map date-line-map)
        time-lines-vec (filter some? (map (partial parse-time-line incomplete-date-ok) time-lines))
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
         (let [prev-date (select-values prev-dm [:year :month :day])
               curr-date (select-values curr-dm [:year :month :day])]
           (if (< 0 (compare curr-date prev-date))
             curr-dm
             (throw (ex-info "Dates are not sorted." {:type :unsorted-dates :prev prev-date :current curr-date})))))
       (last-2-seq {:year 0 :month 0 :day 0} dms)))

(defn parse-file
  [file-name today-date filter-fn]
  (process-file file-name
                (fn [lines-seq]
                  (->> lines-seq
                       calc-file-blocks
                       (map (partial parse-file-block today-date))
                       assert-dates-sorted
                       (filter filter-fn)))))

(defn group-by-weeks
  [days]
  (vals (group-by :week-number days)))

(defn elapsed-str [elapsed]
  (if (= 0 elapsed)
    (format "%6s" "-")
    (format "%3d:%02d" (quot elapsed 60) (mod elapsed 60))))

(defn date-str
  [{d :day m :month}]
  (format "%02d/%02d" m d))

(defn normalize-minutes
  [minutes]
  (- minutes (mod minutes 15)))

(defn normalize-minutes-vec
  [minutes-vec]
  (vec (map normalize-minutes minutes-vec)))

(defn add-vecs
  [a b]
  (vec (map + a b)))

(defn calc-total
  [vec]
  (reduce + 0 vec))

(defn flatten-times
  "Convert a list of date-maps into a flattened seq of maps.
   Each map contains all of the information for a single client-project
   on a single day.  Intended to be used with a dm narrowed to a single week."
  [dms]
  (mapcat (fn [dm] (map (fn [tm] (merge (select-keys dm [:year :month :day :day-number])
                                        (select-keys tm [:client :project :elapsed])
                                        {:key (select-values tm [:client :project])}))
                        (:times dm)))
          dms))

(defn calc-elapsed-for-week
  "Add up the elapsed time for all days in the flattened times into a vec.
   Returns a vector of 7 integers indexed by day-number.
   All :keys SHOULD be identical in the passed in vec but no check is made for this."
  [dv]
  (reduce (fn [totals row]
            (let [day-number (:day-number row)
                  elapsed (:elapsed row)
                  current-elapsed (get totals day-number)]
              (assoc totals day-number (+ elapsed current-elapsed))))
          empty-elapsed-vec
          dv))

(defn days-str
  [dms]
  (let [sorted-day-nums (sort (map :day-number dms))
        sorted-day-names (map day-names sorted-day-nums)
        base-str (reduce (fn [s d] (format "%s  %6s" s d)) "" sorted-day-names)]
    (format "%32s %s" "" base-str)))

(defn dates-str
  [dms]
  (let [sorted-dates (sort-by (fn [dm] (select-values dm [:year :month :day])) dms)
        base-str (reduce (fn [s d] (format "%s  %6s" s (date-str d))) "" sorted-dates)]
    (format "%32s  %s  TOTALS  REPORT" "" base-str)))

(defn project-str
  "Produce a string containing the weekly totals for a single project."
  [project-key elapsed-vec day-numbers]
  (let [elapsed-sub-vec (sub-vec elapsed-vec day-numbers)
        elapsed-total (reduce + 0 elapsed-sub-vec)
        elapsed-line (reduce (fn [s e] (str s "  " (elapsed-str e))) "" elapsed-sub-vec)
        project-str (str (get project-key 0) "," (get project-key 1))
        report-total (calc-total (normalize-minutes-vec elapsed-sub-vec))]
    (format "%-32s  %s  %6s  %6s" project-str elapsed-line (elapsed-str elapsed-total) (elapsed-str report-total))))

(defn totals-str
  [label totals-vec day-numbers]
  (let [totals-sub-vec (sub-vec totals-vec day-numbers)
        grand-total (calc-total totals-sub-vec)
        totals-line (reduce (fn [s e] (str s "  " (elapsed-str e))) "" totals-sub-vec)]
    (format "%-32s  %s  %6s" label totals-line (elapsed-str grand-total))))

(defn compute-report-totals
  [times]
  (let [project-totals (map (fn [week-times] (calc-elapsed-for-week week-times)) (vals times))
        normalized-totals (map normalize-minutes-vec project-totals)
        grand-totals (reduce add-vecs empty-elapsed-vec normalized-totals)]
    grand-totals))

(defn print-week
  [dms]
  (let [all-times (flatten-times dms)
        times (group-by :key all-times)
        totals-vec (calc-elapsed-for-week all-times)
        report-totals-vec (compute-report-totals times)
        sorted-keys (sort (keys times))
        sorted-day-nums (sort (map :day-number dms))]
    (println (days-str dms))
    (println (dates-str dms))
    (doseq [project-key sorted-keys]
      (let [project-times (times project-key)
            elapsed-vec (calc-elapsed-for-week project-times)]
        (println (project-str project-key elapsed-vec sorted-day-nums))))
    (println (totals-str "TOTALS" totals-vec sorted-day-nums))
    (println (totals-str "REPORT" report-totals-vec sorted-day-nums))))

(defn print-report
  [lines]
  (let [label-width (apply max (map (comp  count :label) lines))
        column-width (apply max (flatten (map (comp  count :values) lines)))]
    (println label-width)
    (println column-width)))

(def cli-options
  [["-d" "--date DATE" "Set base date (defaults to today)."
    :parse-fn #(parse-date %)
    :default (current-date)
    :validate [#(>= (compare % (parse-date "01/01/2000")) 0) "Must be in this millenium"]]
   ["-h" "--help" "Print help message and exit."]])

(defn -main
  [& args]
  (let [parsed-opts (parse-opts args cli-options)
        {options :options errors :errors pos-args :arguments} parsed-opts
        base-date (:date options)
        file-name (first pos-args)
        help? (some? (:help options))
        arg-count (count pos-args)]
    (when help?
      (println "usage: time-tracker OPTIONS file-name")
      (println (:summary parsed-opts))
      (System/exit 0))
    (when (or (some? errors)
              (not= 1 arg-count))
      (doseq [msg errors]
        (println msg))
      (when (not= 1 arg-count)
        (println "error: file-name required")
        (println "usage: time-tracker OPTIONS file-name")
        (println (:summary parsed-opts)))
      (System/exit 1))
    (try
      (let [today-date-map (date-to-map (current-date))
            raw-data (parse-file file-name today-date-map (current-cycle? base-date))
            filled-data (fill-missing-days raw-data)
            weeks-data (group-by-weeks filled-data)]
        (doseq [week-data weeks-data]
          (println "")
          (print-week week-data)))
      (catch clojure.lang.ExceptionInfo ex
        (println (str "error: " (ex-data ex)))))))
