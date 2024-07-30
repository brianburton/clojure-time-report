(ns time-report.print
  (:require [malli.generator :as mg]))

(defn- max-width
  "Find max width of a seq of strings."
  [strings]
  (apply max (map count (map str strings))))

(defn- left-pad
  "Pad a number to a width"
  [width num]
  (let [raw (str num)
        raw-width (count raw)
        pad-width (- width raw-width)]
    (str (apply str (repeat pad-width " ")) raw)))

(defn- right-pad
  "Pad a number to a width"
  [width num]
  (let [raw (str num)
        raw-width (count raw)
        pad-width (- width raw-width)]
    (str raw (apply str (repeat pad-width " ")))))

(defn- calc-width
  [options width-keyword width-fn]
  (if (contains? options width-keyword)
    (options width-keyword)
    (width-fn)))

(def format-report-row-def-schema
  [:map
   [:label :string]
   [:data [:vector :string]]
   [:totals [:vector :string]]])

(def format-report-options-schema
  [:map
   [:label-width [:maybe [:int {:min 1 :max 100}]]]
   [:data-width [:maybe [:int {:min 1 :max 12}]]]
   [:totals-width [:maybe [:int {:min 1 :max 12}]]]])

(comment (mg/generate format-report-row-def-schema)
         (mg/generate format-report-options-schema))

(defn format-report
  "Take map {:label \"\" :data [x y] :totals [3 7]} and produce [lines].
   Optional options param can contain hard coded width (e.g. :label-width) for each element."
  ([row-defs] (format-report row-defs {}))
  ([row-defs options]
   (let [label-width (calc-width options :label-width #(max-width (map :label row-defs)))
         data-width (calc-width options :data-width #(+ 2 (max-width (flatten (map :data row-defs)))))
         totals-width (calc-width options :totals-width #(+ 2 (max-width (flatten (map :totals row-defs)))))]
     (map (fn [row]
            (str
             (right-pad label-width (:label row))
             "  "
             (apply str (map (partial left-pad data-width) (:data row)))
             "  "
             (apply str (map (partial left-pad totals-width) (:totals row)))))
          row-defs))))

(defn print-lines
  "print each value on a single line"
  [lines]
  (dorun (map println lines)))

(defn print-report
  "Takes same parameters as format-report, calls that and prints every line."
  ([row-defs] (print-report row-defs {}))
  ([row-defs options]
   (print-lines (format-report row-defs options))))
