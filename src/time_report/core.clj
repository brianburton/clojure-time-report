(ns time-report.core
  (:require [clojure.set :as set]
            [java-time.api :as jt]))

(defn select-values
  "Create a vector containing value for each key (or nil if not in map)."
  [source-map keys]
  (vec (map #(get source-map %) keys)))

(defn sub-vec
  "Create a vector containing only the values with the given indexes from the source vector."
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

(defmacro catch-and-return [f]
  `(try
     ~f
     (catch clojure.lang.ExceptionInfo e1#
       (assoc (ex-data e1#) :message (ex-message e1#)))
     (catch Exception e2# (hash-map :type :unknown :class (class e2#) :message (.getMessage e2#)))))

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

(defn prior-date [date days-ago] (jt/minus date (jt/days days-ago)))

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

(defn cycle-days-range
  "Return a tuple containing first and last day numbers (1-15 or 16-N) in the given cycle."
  [year month day]
  (if (< day 16)
    [1 15]
    [16 (days-in-month year month)]))

(defn closed-range
  "Like range but to is inclusive."
  [from to] (range from (inc to)))

(defn cycle-days
  "Return a set containing all of the day numbers (1-15 or 16-N) in the given cycle."
  [year month day]
  (let [[first last] (cycle-days-range year month day)]
    (set (range first (inc last)))))

(defn same-cycle?
  "Return a function that takes a date-map and returns true if the date from the map is in the same cycle as the given date."
  [year month day]
  (let [[first-day last-day] (cycle-days-range year month day)]
    (fn [{y :year m :month d :day}]
      (and
       (= year y)
       (= month m)
       (<= first-day d last-day)))))

(defn current-cycle?
  "Given a date-map to identify a cycle, return a function that takes another date-map and returns true if that date is in the target cycle."
  [today-map]
  (let [{current-year :year current-month :month current-day :day} today-map]
    (same-cycle? current-year current-month current-day)))

(defn fill-missing-days
  "Given a sequence of date maps from a single cycle return a sorted vector of date maps including zero-elapsed date-maps for any days missing from the input sequence."
  [dms]
  (let [{:keys [year month day]} (first dms)
        present-days (set (map :day dms))
        all-days (cycle-days year month day)
        missing-days (set/difference all-days present-days)
        missing-dms (map (fn [d]
                           (let [ld (jt/local-date year month d)
                                 dn (day-of-week ld)
                                 wn (week-number ld)]
                             {:year year :month month :day d :day-number dn :week-number wn :times []}))
                         missing-days)]
    (vec (sort-by :day (concat dms missing-dms)))))

(defn same-date?
  "Given two date maps return true if they represent the same date."
  [a b]
  (apply = (map #(select-values % [:year :month :day]) [a b])))

(defn normalize-minutes
  "Truncates an elapsed time to an even multiple of 15 minutes."
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
