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
      {:type    :invalid-time-string
       :text    "bad"
       :message "invalid time string"}
      "bad"
      {:type    :unknown
       :class   java.lang.NullPointerException
       :message nil}
      nil))
  (testing "valid time"
    (is (= (LocalTime/of 11 33) (parse-time "1133")))))

(deftest parse-date-test
  (testing "invalid dates throw"
    (are [exinfo date-str] (= exinfo (catch-and-return (parse-date date-str)))
      {:type    :invalid-date-string
       :text    "bad"
       :message "invalid date string"}
      "bad"
      {:type    :unknown
       :class   java.lang.NullPointerException
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