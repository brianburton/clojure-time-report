(defproject time-report "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 ;; https://mvnrepository.com/artifact/clojure.java-time/clojure.java-time
                 [clojure.java-time/clojure.java-time "1.4.2"]
                 ;; https://mvnrepository.com/artifact/org.clojure/data.json
                 [org.clojure/data.json "2.5.0"]
                 [org.clojure/tools.cli "1.1.230"]]
  :main ^:skip-aot time-report.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
