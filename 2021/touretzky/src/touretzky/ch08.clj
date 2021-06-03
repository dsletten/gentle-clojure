;;;;
;;;;
;;;;   Programming is not about typing, it's about thinking.
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               ch08.clj
;;;;
;;;;   Started:            Wed Jun  2 20:30:56 2021
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

(ns ch08
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    8.2
;;;
(defn anyodd? [seq]
  (cond (empty? seq) false
        (odd? (first seq)) true
        :else (anyodd? (rest seq))))

(defn anyodd? [seq]
  (if (empty? seq)
    false
    (if (odd? (first seq))
      true
      (anyodd? (rest seq)))) )

(deftest test-anyodd?
  (is (not (anyodd? '())))
  (is (anyodd? '(7)))
  (is (not (anyodd? '(6))))
  (is (anyodd? '(6 7)))
  (is (anyodd? '(2 4 6 7 8 9))))

;;;
;;;    8.4
;;;
(defn laugh [n]
  (cond (zero? n) '()
        :else (cons '☺ (laugh (dec n)))) )
;αβγ

(deftest test-laugh
  (is (= (laugh 0) '()))
  (is (= (laugh 1) '(☺)))
  (is (= (laugh 5) '(☺ ☺ ☺ ☺ ☺))))

;;;
;;;    8.5
;;;
(defn add-up [ns]
  (cond (empty? ns) 0
        :else (+ (first ns) (add-up (rest ns)))) )

(deftest test-add-up
  (is (zero? (add-up '())))
  (is (== (add-up '(2 3 7)) 12))
  (is (zero? (add-up '(0 0 0 0))))
  (is (zero? (add-up '(1 -1 2 -2 3 -3 4 -4)))) )

;;;
;;;    8.6
;;;
(defn allodd? [ns]
  (cond (empty? ns) true
        (odd? (first ns)) (allodd? (rest ns))
        :else false))

(defn allodd? [ns]
  (cond (empty? ns) true
        (even? (first ns)) false
        :else (allodd? (rest ns))))

(deftest test-allodd?
  (is (allodd? '()))
  (is (allodd? '(1)))
  (is (allodd? '(1 3 5 7 9)))
  (is (not (allodd? '(2))))
  (is (not (allodd? '(1 2 3)))) )

;;;
;;;    8.7
;;;
