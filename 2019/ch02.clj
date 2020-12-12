;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch02.clj
;;;;
;;;;   Started:            Sat Mar 28 05:46:05 2020
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

(ns ch02
  (:use clojure.test
        clojure.math.numeric-tower
        [clojure.pprint :only (cl-format)]))

;;;
;;;    2.21
;;;    
(defn pack [a b c d]
  (list (list a b) (list c d)))

(deftest test-pack
  (is (= (pack 1 2 3 4) '((1 2) (3 4))))
  (is (= (pack '(a) 'b 'c '(d)) '(((a) b) (c (d)))) ))

;;;
;;;    2.22
;;;    
(defn duo-cons [x y l]
  (cons x (cons y l)))

(deftest test-duo-cons
  (is (= (duo-cons 'patrick 'seymour '(marvin)) '(patrick seymour marvin)))
  (is (= (duo-cons '(a) '(b) '((c))) '((a) (b) (c)))) )

;;;
;;;    2.23
;;;    
(defn two-deeper [obj]
  (list (list obj)))

(defn two-deeper [obj]
  (cons (cons obj '()) '()))

(deftest test-two-deeper
  (is (= (two-deeper 'moo) '((moo))))
  (is (= (two-deeper '(bow wow)) '(((bow wow)))) ))
