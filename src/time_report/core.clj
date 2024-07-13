(ns time-report.core
  (:require [time-report.print :as pr])
  (:require [clojure.set :as set])
  (:require [java-time.api :as jt])
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(defn select-values
  "Create a vector containing value for each key (or nil if not in map)."
  [source-map keys]
  (vec (map #(get source-map %) keys)))

(defn sub-vec
  [v indexes]
  (reduce (fn [newv i] (conj newv (v i))) [] indexes))

(defn consecutive-items-seq
  "Creates a lazy seq that reads from the provided seq and returns
   pairs of previous value plus current value.  Initial value can be
   provided when calling (nil used if none provided)."
  ([the-seq] (consecutive-items-seq nil the-seq))
  ([prev-value the-seq]
   (lazy-seq
    (when-not (empty? the-seq)
      (let [this-value (first the-seq)]
        (cons [prev-value this-value] (consecutive-items-seq this-value (rest the-seq))))))))

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
; (defn day-of-week [day] (let [d (.. day getDayOfWeek getValue)] (if (>= d 6) (- d 6) (+ d 1))))

; monday first order:
(def ^:const day-names ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])
(def ^:const day-abbrevs ["MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN"])
(defn day-of-week [day] (let [d (.. day getDayOfWeek getValue)] (dec d)))

(defn day-name [d] (day-names (day-of-week d)))
(defn day-abbrev [d] (day-abbrevs (day-of-week d)))

(defn day-index
  "Computes an index that uniquely identifies the given date."
  [day]
  (let [raw-base (jt/local-date 1996 1 1)
        real-base (jt/minus raw-base (jt/days (day-of-week raw-base)))]
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
  "Return a set containing all of the day numbers (1-15 or 16-N) in the given cycle."
  [year month day]
  (if (< day 16)
    (set (range 1 16))
    (set (range 16 (+ 1 (days-in-month year month))))))

(defn same-cycle?
  "Return a function that takes a date-map and returns true if the date from the map is in the same cycle as the given date."
  [year month day]
  (let [good-days (cycle-days year month day)]
    (fn [{y :year m :month d :day}]
      (and
       (= year y)
       (= month m)
       (good-days d)))))

(defn current-cycle?
  "Given a date-map to identify a cycle, return a function that takes another date-map and returns true if that date is in the target cycle."
  [today-map]
  (let [{current-year :year current-month :month current-day :day} today-map]
    (same-cycle? current-year current-month current-day)))

(defn prev-cycle?
  "Given a date-map to identify a cycle, return a function that takes another date-map and returns true if that date is in the cycle previous to the target cycle."
  [today-map]
  (let [{current-year :year current-month :month current-day :day} today-map
        prev-day (if (< current-day 16) 16 1)
        prev-month (if (< current-day 16)
                     (if (= current-month 1) 12 (dec current-month))
                     current-month)
        prev-year (if (< current-day 16)
                    (if (= current-month 1) (dec current-year) current-year)
                    current-year)]
    (same-cycle? prev-year prev-month prev-day)))

(defn fill-missing-days
  "Given a sequence of date maps from a single cycle return a sorted vector of date maps including zero-elapsed date-maps for any days missing from the input sequence."
  [dms]
  (let [present-days (set (map :day dms))
        first-dm (first dms)
        {year :year month :month day :day} first-dm
        all-days (cycle-days year month day)
        missing-days (set/difference all-days present-days)
        missing-dms (map (fn [d]
                           (let [ld (jt/local-date year month d)
                                 dn (day-of-week ld)
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
       :day-number (day-of-week d)
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
    (doseq [[prev curr] (consecutive-items-seq "0000" all-times)]
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
        incomplete-time-span-ok (same-date? today-date-map date-line-map)
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
         (let [prev-date (select-values prev-dm [:year :month :day])
               curr-date (select-values curr-dm [:year :month :day])]
           (if (< 0 (compare curr-date prev-date))
             curr-dm
             (throw (ex-info "Dates are not sorted." {:type :unsorted-dates :prev prev-date :current curr-date})))))
       (consecutive-items-seq {:year 0 :month 0 :day 0} dms)))

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
  "Truncats an elapsed time to an even multiple of 15 minutes."
  [minutes]
  (- minutes (mod minutes 15)))

(defn normalize-minutes-vec
  "Normalizes all elapsed times in a vector."
  [minutes-vec]
  (vec (map normalize-minutes minutes-vec)))

(defn add-vecs
  [a b]
  (vec (map + a b)))

(defn calc-total
  "Compute simple total of numbers in a vector."
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
   Returns a vector of 7 totals indexed by day-number."
  [dv]
  (reduce (fn [totals row]
            (let [day-number (:day-number row)
                  elapsed (:elapsed row)
                  current-elapsed (get totals day-number)]
              (assoc totals day-number (+ elapsed current-elapsed))))
          empty-elapsed-vec
          dv))

(defn compute-report-totals
  [times]
  (let [project-totals (map (fn [week-times] (calc-elapsed-for-week week-times)) (vals times))
        normalized-totals (map normalize-minutes-vec project-totals)
        grand-totals (reduce add-vecs empty-elapsed-vec normalized-totals)]
    grand-totals))

(defn days-row
  [dms]
  (let [sorted-day-nums (sort (map :day-number dms))
        sorted-day-names (map day-abbrevs sorted-day-nums)]
    {:label ""
     :data sorted-day-names
     :totals ["" ""]}))

(defn dates-row
  [dms]
  (let [sorted-dms (sort-by (fn [dm] (select-values dm [:year :month :day])) dms)
        sorted-dates (map date-str sorted-dms)]
    {:label "PROJECT"
     :data sorted-dates
     :totals ["TOTALS" "REPORT"]}))

(defn project-str
  [[client project]]
  (str client "," project))

(defn project-row
  [project-times-by-key day-numbers project-key]
  (let [project-times (project-times-by-key project-key)
        elapsed-vec (calc-elapsed-for-week project-times)
        elapsed-sub-vec (sub-vec elapsed-vec day-numbers)
        elapsed-total (calc-total elapsed-sub-vec)
        report-total (calc-total (normalize-minutes-vec elapsed-sub-vec))]
    {:label (project-str project-key)
     :data (map elapsed-str elapsed-sub-vec)
     :totals (map elapsed-str [elapsed-total report-total])}))

(defn totals-row
  [label totals-vec day-numbers]
  (let [totals-sub-vec (sub-vec totals-vec day-numbers)
        grand-total (calc-total totals-sub-vec)]
    {:label label
     :data (map elapsed-str totals-sub-vec)
     :totals [(elapsed-str grand-total)]}))

(defn calc-max-label-length
  [project-keys]
  (apply max 0 (map #(count (project-str %)) project-keys)))

(defn unique-project-keys
  [dms]
  (->> dms
       (map :times)
       (flatten)
       (map #(select-values % [:client :project]))
       (distinct)))

(defn create-report-for-week
  [dms formatter-options]
  (let [all-times (flatten-times dms)
        project-times-by-key (group-by :key all-times)
        simple-totals-vec (calc-elapsed-for-week all-times)
        report-totals-vec (compute-report-totals project-times-by-key)
        sorted-project-keys (sort (keys project-times-by-key))
        sorted-day-nums (sort (map :day-number dms))
        project-rows (map (partial project-row project-times-by-key sorted-day-nums) sorted-project-keys)
        simple-totals (totals-row "TOTALS" simple-totals-vec sorted-day-nums)
        report-totals (totals-row "REPORT" report-totals-vec sorted-day-nums)]
    (pr/format-report
     (flatten [(days-row dms) (dates-row dms) project-rows simple-totals report-totals])
     formatter-options)))
