;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch03.clj
;;;;
;;;;   Started:            Mon May 13 02:29:05 2013
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

(ns ch03
  (:use clojure.test
        clojure.math.numeric-tower
        [clojure.pprint :only (cl-format)]))

;;;
;;;    3.6
;;;
(defn pythagorean [a b]
  (sqrt (+ (* a a) (* b b))))

;;;
;;;    3.7
;;;
(defn miles-per-gallon [initial-odometer-reading final-odometer-reading gallons-consumed]
  (/ (- final-odometer-reading initial-odometer-reading) gallons-consumed))

;;;
;;;    3.11
;;;
(defn longer-than? [l1 l2]
  (cond (empty? l1) false
        (empty? l2) true
        :else (longer-than? (rest l1) (rest l2))))

(defn longer-than? [l1 l2]
  (cond (empty? l1) false
        (empty? l2) true
        :else (recur (rest l1) (rest l2))))

;;;
;;;    3.12
;;;
(defn addlength [l]
  (cons (count l) l))

;;;
;;;    3.22 d
;;;
(defn first? [sym l]
  (= sym (first l)))

(deftest test-first?
  (is (first? 'foo '(foo bar baz)))
  (is (not (first? 'boing '(foo bar baz)))) )

;;;
;;;    3.22 e
;;;
(defn mid-add1 [l]
  (cons (first l)
        (cons (inc (second l))
              (rest (rest l)))) )

(defn mid-add1 [[first second & rest]]
  (list* first (inc second) rest))

(deftest test-mid-add1
  (is (= (mid-add1 '(take 2 cookies)) '(take 3 cookies))))

;;;
;;;    3.22 f
;;;
(defn f-to-c [f]
  (* 5/9 (- f 32)))

(defn c-to-f [c]
  (+ (* 9/5 c) 32))

(deftest test-f-to-c
  (is (== (f-to-c 32) 0N))
  (is (==  (f-to-c -40) -40N))
  (is (==  (f-to-c 212) 100N))
  (is (==  (f-to-c 98.6) 37.0)))

(deftest test-c-to-f
  (is (==  (c-to-f 0) 32N))
  (is (==  (c-to-f -40) -40N))
  (is (==  (c-to-f 100) 212N))
  (is (==  (c-to-f 37) 493/5)))
