;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               unary.clj
;;;;
;;;;   Started:            Thu May  9 02:03:01 2013
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

(ns unary
  (:use clojure.test
        clojure.math.numeric-tower
        [clojure.pprint :only (cl-format)]))

(def ^:const tally 'X)

(defn number [n]
  (count n))

(deftest test-number
  (is (clojure.core/== (number '()) 0))
  (is (clojure.core/== (number '(X)) 1))
  (is (clojure.core/== (number '(X X)) 2))
  (is (clojure.core/== (number '(X X X)) 3)))

(defn unary [n]
  (repeat n tally))

(deftest test-unary
  (is (= (unary 0) '()))
  (is (= (unary 1) '(X))) 
  (is (= (unary 2) '(X X))) 
  (is (= (unary 3) '(X X X))))

(defn zero? [n]
  (empty? n))

(deftest test-zero?
  (is (zero? (unary 0)))
  (is (not (zero? (unary 1))))
  (is (not (zero? (unary 2))))
  (is (not (zero? (unary 3)))) )
  
(defn inc [n]
  (cons tally n))

(deftest test-inc
  (is (clojure.core/== (number (inc (unary 0))) (clojure.core/inc 0)))
  (is (clojure.core/== (number (inc (unary 1))) (clojure.core/inc 1)))
  (is (clojure.core/== (number (inc (unary 2))) (clojure.core/inc 2)))
  (is (clojure.core/== (number (inc (unary 3))) (clojure.core/inc 3))))
  
(defn dec [n]
  (if (zero? n)
    'error
    (rest n)))

(deftest test-dec
  (is (= (dec (unary 0)) 'error))
  (is (clojure.core/== (number (dec (unary 1))) (clojure.core/dec 1)))
  (is (clojure.core/== (number (dec (unary 2))) (clojure.core/dec 2)))
  (is (clojure.core/== (number (dec (unary 3))) (clojure.core/dec 3))))
  
(def ^:const zero '())
(def ^:const one (inc zero))
(def ^:const two (inc one))

(defn == [m n]
  (cond (zero? m) (zero? n)
        (zero? n) false
        :else (recur (dec m) (dec n))))
;;         :else (== (dec m) (dec n))))

(deftest test-==
  (is (== (unary 5) (inc (unary 4))))
  (is (== (unary 5) (dec (unary 6))))
  (is (not (== (unary 5) (unary 4)))) )

(defn > [m n]
  (cond (zero? m) false
        (zero? n) true
        :else (recur (dec m) (dec n))))
;;         :else (> (dec m) (dec n))))

(deftest test->
  (is (> (unary 5) (unary 4)))
  (is (> (unary 2) (unary 0)))
  (is (not (> (unary 5) (unary 6)))) )
  
(defn < [m n]
  (cond (zero? n) false
        (zero? m) true
        :else (recur (dec m) (dec n))))
;;         :else (< (dec m) (dec n))))

(deftest test-<
  (is (< (unary 4) (unary 5)))
  (is (< (unary 0) (unary 2)))
  (is (not (< (unary 6) (unary 5)))) )
  
(defn + [m n]
  (if (zero? n)
    m
    (recur (inc m) (dec n))))
;;     (+ (inc m) (dec n))))

(deftest test-+
  (is (clojure.core/== (number (+ (unary 1) (unary 2))) (clojure.core/+ 1 2)))
  (is (clojure.core/== (number (+ (unary 2) (unary 1))) (clojure.core/+ 2 1)))
  (is (clojure.core/== (number (+ (unary 2) (unary 0))) (clojure.core/+ 2 0)))
  (is (clojure.core/== (number (+ (unary 0) (unary 2))) (clojure.core/+ 0 2))))
  
(defn - [m n]
  (cond (zero? n) m
        (zero? m) 'error
        :else (recur (dec m) (dec n)))) ; Craig Andera
;;         :else (- (dec m) (dec n))))

(deftest test--
  (is (= (- (unary 1) (unary 2)) 'error))
  (is (clojure.core/== (number (- (unary 2) (unary 1))) (clojure.core/- 2 1)))
  (is (clojure.core/== (number (- (unary 2) (unary 0))) (clojure.core/- 2 0))))
  
(defn * [m n]
  (cond (zero? n) zero
        (zero? m) zero
        :else (+ m (* m (dec n)))) )

(deftest test-*
  (is (clojure.core/== (number (* (unary 1) (unary 2))) (clojure.core/* 1 2)))
  (is (clojure.core/== (number (* (unary 2) (unary 1))) (clojure.core/* 2 1)))
  (is (clojure.core/== (number (* (unary 2) (unary 0))) (clojure.core/* 2 0)))
  (is (clojure.core/== (number (* (unary 0) (unary 2))) (clojure.core/* 0 2))))
  
(defn / [m n]
  (cond (zero? n) 'error
        (< m n) zero
        :else (inc (/ (- m n) n))))

(deftest test-/
  (is (= (/ (unary 1) (unary 0)) 'error))
  (is (clojure.core/== (number (/ (unary 8) (unary 4))) (clojure.core// 8 4)))
  (is (clojure.core/== (number (/ (unary 2) (unary 1))) (clojure.core// 2 1)))
  (is (clojure.core/== (number (/ (unary 5) (unary 2))) (clojure.core// 2 1)))
  (is (clojure.core/== (number (/ (unary 2) (unary 1))) (clojure.core// 2 1)))
  (is (clojure.core/== (number (/ (unary 2) (unary 0))) (clojure.core// 2 0))))
  
(defn mod [m n]
  (cond (zero? n) 'error
        (< m n) m
        :else (recur (- m n) n)))
;;         :else (mod (- m n) n)))

(defn pos? [n]
  (> n zero))

(defn even? [n]
  (zero? (mod n two)))

(defn odd? [n]
  (not (even? n)))
