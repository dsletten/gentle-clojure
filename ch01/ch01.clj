;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch01.clj
;;;;
;;;;   Started:            Thu May  2 01:59:04 2013
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

(ns ch01
  (:use clojure.math.numeric-tower
        clojure.test
        [clojure.pprint :only (cl-format)]))

(defn add1 [x]
  (+ x 1))

(defn add2 [x]
  (add1 (add1 x)))

(defn two? [x]
  (== x 2))

(defn sub2 [x]
  (- x 2))

(defn two? [x]
  (zero? (sub2 x)))

(defn half [x]
  (* x 0.5))

(defn half [x]
  (/ x 2))

(defn multi-digit? [x]
  (> x 9))

(defn negate [x]
  (- x))

(defn one-more? [x y]
  (== x (add1 y)))

(defn twomore? [x y]
  (== x (add2 y)))

(defn twomore? [x y]
  (== (sub2 x) y))

(defn average [x y]
  (half (+ x y)))

(defn more-than-half? [x y]
  (> x (half y)))

(defn not-one? [x]
  (not (== x 1)))

(defn not-plus? [x]
  (not (> x 0)))

(defn evenp [x]
  (not (odd? x)))

(defn xor [x y]
  (not (= x y)))
