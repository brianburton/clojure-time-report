(ns time-report.main
  (:gen-class)
  (:require [time-report.core :refer [parse-date current-date]]
            [time-report.mode.random :as random]
            [time-report.mode.weekly :as weekly]
            [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-d" "--date DATE" "Set base date (defaults to today)."
    :parse-fn #(parse-date %)
    :default (current-date)
    :validate [#(>= (compare % (parse-date "01/01/2000")) 0) "Must be in this millenium"]]
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
        "random" (random/execute)
        "weekly" (weekly/execute base-date file-name))
      (catch clojure.lang.ExceptionInfo ex
        (println (str "error: " (ex-data ex)))))))
