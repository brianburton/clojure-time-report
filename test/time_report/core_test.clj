(ns time-report.core-test
  (:require [clojure.test :refer :all]
            [time-report.core :refer :all]))
(import (java.time LocalDate LocalTime))

(deftest select-values-test
  (testing "selecting values"
    (are [expected m ks] (= expected (select-values m ks))
      [] {} []
      [nil] {:a 1} [:b]
      [1] {:a 1} [:a]
      [1 nil] {:a 1} [:a :b]
      [nil 1] {:b 1} [:a :b])))

(deftest sub-vec-test
  (testing "vector contains only specified indexed vales"
    (are [expected v ks] (= expected (sub-vec v ks))
      [] [] []
      [] [10 11 12] []
      [10 12] [10 11 12] [0 2]
      [11 10 12] [10 11 12] [1 0 2])))

(deftest consecutive-items-seq-test
  (testing "correct pairs are returned"
    (are [expected p s] (= expected (consecutive-items-seq p s))
      [] nil []
      [[nil 1]] nil [1]
      [[0 1]] 0 [1]
      [[0 1] [1 2] [2 3]] 0 [1 2 3])))

(deftest parse-time-test
  (testing "invalid times throw"
    (are [exinfo time-str] (= exinfo (catch-and-return (parse-time time-str)))
      {:type :invalid-time-string
       :text "bad"
       :message "invalid time string"}
      "bad"
      {:type :unknown
       :class java.lang.NullPointerException
       :message nil}
      nil))
  (testing "valid time"
    (is (= (LocalTime/of 11 33) (parse-time "1133")))))

(deftest parse-date-test
  (testing "invalid dates throw"
    (are [exinfo date-str] (= exinfo (catch-and-return (parse-date date-str)))
      {:type :invalid-date-string
       :text "bad"
       :message "invalid date string"}
      "bad"
      {:type :unknown
       :class java.lang.NullPointerException
       :message nil}
      nil))
  (testing "valid date"
    (is (= (LocalDate/of 1997 11 18) (parse-date "11/18/1997")))))

(deftest elapsed-test
  (testing "match is correct"
    (are [expected time1 time2] (= expected (elapsed (parse-time time1) (parse-time time2)))
      0 "1112" "1112"
      1 "1112" "1113"
      87 "1700" "1827")))

(deftest day-of-week-test
  (are [y m d dow] (= dow (day-of-week (LocalDate/of y m d)))
    2023 1 2 0
    2023 5 2 1
    2023 5 24 2
    2023 7 20 3
    2023 8 4 4
    2024 1 6 5
    2024 9 8 6))

(deftest day-name-test
  (are [y m d dow] (= dow (day-name (LocalDate/of y m d)))
    2023 1 2 "Monday"
    2023 5 2 "Tuesday"
    2023 5 24 "Wednesday"
    2023 7 20 "Thursday"
    2023 8 4 "Friday"
    2024 1 6 "Saturday"
    2024 9 8 "Sunday"))

(deftest day-abbrev-test
  (are [y m d dow] (= dow (day-abbrev (LocalDate/of y m d)))
    2023 1 2 "MON"
    2023 5 2 "TUE"
    2023 5 24 "WED"
    2023 7 20 "THU"
    2023 8 4 "FRI"
    2024 1 6 "SAT"
    2024 9 8 "SUN"))

(deftest day-index-test
  (testing "day index is aligned with day of week"
    (are [y m d] (let [date (LocalDate/of y m d)] (= (mod (day-index date) 7) (day-of-week date)))
      2023 1 2
      2023 5 2
      2023 5 24
      2023 7 20
      2023 8 4
      2024 1 6
      2024 9 8)))

(deftest prior-date-test
  (testing "computes date properly"
    (are [y1 m1 d1 y2 m2 d2 off] (= (LocalDate/of y1 m1 d1) (prior-date (LocalDate/of y2 m2 d2) off))
      2024 2 27 2024 3 8 10
      2023 12 20 2024 1 7 18)))

(deftest date-to-map-test
  (is (= {:year 2023 :month 12 :day 30} (date-to-map (LocalDate/of 2023 12 30)))))

(deftest leap-year?-test
  (testing "eap years"
    (are [y leap?] (= leap? (leap-year? y))
      1904 true
      1905 false
      1906 false
      1907 false
      1908 true
      1700 false
      1800 false
      1900 false
      2000 true)))

(deftest days-in-month-test
  (testing "normal year"
    (are [month days] (= days (days-in-month 2023 month))
      1 31
      2 28
      3 31
      4 30
      5 31
      6 30
      7 31
      8 31
      9 30
      10 31
      11 30
      12 31))
  (testing "lear year" (is (= 29 (days-in-month 2024 2)))))

(deftest closed-range-test
  (is (= [3 4 5] (vec (closed-range 3 5)))))

(deftest cycle-days-range-test
  (let [start [1 15]
        end-28 [16 28]
        end-29 [16 29]
        end-30 [16 30]
        end-31 [16 31]]
    (are [y m d expected] (= expected (cycle-days-range y m d))
      2024 1 1 start
      2024 1 15 start
      2024 1 16 end-31
      2024 2 1 start
      2024 2 15 start
      2023 2 28 end-28
      2024 2 28 end-29
      2024 2 29 end-29
      2024 3 1 start
      2024 3 16 end-31
      2024 4 15 start
      2024 4 16 end-30)))

(deftest cycle-days-test
  (let [start (set (range 1 16))
        end-29 (set (range 16 30))
        end-30 (set (range 16 31))
        end-31 (set (range 16 32))]
    (are [m d expected] (= expected (cycle-days 2024 m d))
      1 1 start
      1 15 start
      1 16 end-31
      2 1 start
      2 15 start
      2 28 end-29
      2 29 end-29
      3 1 start
      3 16 end-31
      4 15 start
      4 16 end-30)))

(deftest current-cylce?-test
  (let [y 2024
        m 3
        d 14
        in-cycle? (current-cycle? {:year y :month m :day d})]
    (testing "true for days in cycle"
      (doseq [d1 (range 1 16)] (is (in-cycle? {:year y :month m :day d1}))))
    (testing "false for days before cycle"
      (doseq [d1 (range 1 16)] (is (not (in-cycle? {:year y :month (- m 1) :day d1})))))
    (testing "false for days after cycle"
      (doseq [d1 (range 17 32)] (is (not (in-cycle? {:year y :month m :day d1})))))))

(deftest fill-missing-days-test
  (let [provided {:year 2024 :month 3 :day 23 :day-number 5 :times [1 2 3]}
        before [provided]
        after (fill-missing-days before)]
    (is (= (.indexOf after provided) 7))
    (is (= (first after) {:year 2024 :month 3 :day 16 :day-number 5 :week-number 1471 :times []}))
    (is (= (map :year after) (repeat 16 2024)))
    (is (= (map :month after) (repeat 16 3)))
    (is (= (map :day after) (closed-range 16 31)))
    (is (= (map :day-number after) (take 16 (cycle [5 6 0 1 2 3 4]))))))

(deftest normalize-minutes-vec-test
  (is (= [60 75 75 75 90] (normalize-minutes-vec [74 75 76 89 90]))))

(deftest add-vecs-test
  (is (= [] (add-vecs [] [])))
  (is (= [3] (add-vecs [1] [2]))))
