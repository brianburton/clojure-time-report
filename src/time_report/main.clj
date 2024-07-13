(ns time-report.main
  (:gen-class)
  (:require [time-report.core :as core])
  (:require [time-report.print :as pr])
  (:require [time-report.random :as rnd])
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-d" "--date DATE" "Set base date (defaults to today)."
    :parse-fn #(core/parse-date %)
    :default (core/current-date)
    :validate [#(>= (compare % (core/parse-date "01/01/2000")) 0) "Must be in this millenium"]]
   ["-m" "--mode MODE" "Select the execution mode."
    :default "weekly"
    :validate [#(contains? #{"weekly" "random"} %) "unknown mode"]]
   ["-h" "--help" "Print help message and exit."]])

(defn -main
  [& args]
  (let [parsed-opts (parse-opts args cli-options)
        {options :options errors :errors pos-args :arguments} parsed-opts
        mode (:mode options)
        base-date (:date options)
        file-name (first pos-args)
        help? (some? (:help options))
        arg-count (count pos-args)
        missing-file-name (fn [] (and (= mode "weekly") (not= 1 arg-count)))]
    (when help?
      (println "usage: time-tracker OPTIONS file-name")
      (println (:summary parsed-opts))
      (System/exit 0))
    (when (or (some? errors)
              (missing-file-name))
      (doseq [msg errors]
        (println msg))
      (when (missing-file-name)
        (println "error: file-name required")
        (println "usage: time-tracker OPTIONS file-name")
        (println (:summary parsed-opts)))
      (System/exit 1))
    (try
      (case mode
        "random" (pr/print-lines (rnd/random-data-file))
        "weekly" (let [base-date-map (core/date-to-map base-date)
                       today-date-map (core/date-to-map (core/current-date))
                       raw-data (core/parse-file file-name today-date-map (core/current-cycle? base-date-map))
                       filled-data (core/fill-missing-days raw-data)
                       weeks-data (core/group-by-weeks filled-data)
                       labels-width (core/calc-max-label-length (core/unique-project-keys filled-data))
                       printer-options {:label-width labels-width}]
                   (println printer-options)
                   (println (core/unique-project-keys filled-data))
                   (doseq [week-data weeks-data]
                     (println "")
                     (pr/print-lines (core/create-report-for-week week-data printer-options)))))
      (catch clojure.lang.ExceptionInfo ex
        (println (str "error: " (ex-data ex)))))))
