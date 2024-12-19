;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch03_test.clj
;;;;
;;;;   Started:            Mon Sep 16 18:57:11 2024
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;

(ns touretzky.ch03-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [are deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [touretzky.ch03 :as ch03]))

(st/instrument)

(deftest test-average
  (is (== 0 (ch03/average 0 0)))
  (is (== 0.5 (ch03/average 0 1)))
  (is (== 1/2 (ch03/average 0 1)))
  (is (== 1 (ch03/average 0 2)))
  (is (== 8 (ch03/average 8 8))))

(defn approximately=
  ([a b] (approximately= a b 1e-6))
  ([a b epsilon]
   (<= (Math/abs (- a b)) (* epsilon (Math/abs a)))) )

(deftest test-cube
  (is (== 125 (ch03/cube 5)))
  (is (== 27.0 (ch03/cube 3.0)))
  (is (== -8 (ch03/cube -2)))
  (is (zero? (ch03/cube 0)))
  (is (== 8 (ch03/cube 2)))
  (is (== -27 (ch03/cube -3)))
  (is (approximately= (ch03/cube Math/PI) 31.006276680299816)))

(deftest test-pythag
  (is (== 5 (ch03/pythag 3 4)))
  (is (== 5.0 (ch03/pythag 3.0 4.0)))
  (is (== 13 (ch03/pythag 5 12)))
  (is (== 13.0 (ch03/pythag 5.0 12.0))))

(deftest test-miles-per-gallon
  (is (== 70/3 (ch03/miles-per-gallon 0 420 18)))
  (is (== 23.35 (ch03/miles-per-gallon 0 420.3 18))))

(deftest test-longer?
  (is (ch03/longer? '(a b c) '(a b)))
  (is (ch03/longer? '(a b c) '()))
  (is (not (ch03/longer? '(a b) '(a b c))))
  (is (not (ch03/longer? '(a b c) '(a b c))))
  (is (ch03/longer? (range 10) (range 5)))
  (is (ch03/longer? [1 2 3 4] [1 2 3])))

(deftest test-add-length
  (is (= '(0) (ch03/add-length [])))
  (is (= '(1 0) (ch03/add-length (ch03/add-length []))))
  (is (= '(3 a b c) (ch03/add-length '(a b c))))
  (is (= '(4 moo goo gai pan) (ch03/add-length '(moo goo gai pan))))
  (is (= '(4 3 a b c) (ch03/add-length (ch03/add-length '(a b c)))) ))
