(ns time-report.mode.weekly
  (:require [time-report.core :refer :all]
            [time-report.parse :as parse]
            [time-report.print :as pr]))

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

(defn execute
  [base-date file-name]
  (let [base-date-map (date-to-map base-date)
        today-date-map (date-to-map (current-date))
        raw-data (parse/parse-file file-name today-date-map (current-cycle? base-date-map))
        filled-data (fill-missing-days raw-data)
        weeks-data (group-by-weeks filled-data)
        labels-width (calc-max-label-length (unique-project-keys filled-data))
        printer-options {:label-width labels-width}]
    (println printer-options)
    (println (unique-project-keys filled-data))
    (doseq [week-data weeks-data]
      (println "")
      (pr/print-lines (create-report-for-week week-data printer-options)))))