;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               unary.clj
;;;;
;;;;   Started:            Sat Dec 12 00:51:49 2020
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
        [clojure.pprint :only (cl-format)]))

(declare ==)

(def ^:const tally 'X)

(defn unary [n]
  (repeat n tally))

(defn unary? [n]
  (every? (fn [elt] (= elt tally)) n))

(defn number [unary]
  (count unary))

(def zero (unary 0))

;; (defn zero? [n]
;;   (== n zero)) ; Infinite recursion! zero? and == are mutually dependent here!

(defn zero? [n]
  (empty? n))

(deftest test-zero?
  (is (zero? zero))
  (is (not (zero? '(X)))) )

(defn inc [n]
  (cons tally n))

(def one (inc zero))
(def two (inc one))

(deftest test-inc
  (is (== (inc zero) '(X)))
  (is (== (inc (inc zero)) '(X X))))

(defn dec [n]
  (if (zero? n)
    (throw (IllegalArgumentException. (cl-format nil "Cannot decrement ~A" n)))
    (rest n)))

(deftest test-dec
  (is (zero? (dec '(X))))
  (is (zero? (dec (inc zero)))) )

(defn == [m n]
  (cond (zero? m) (zero? n)
        (zero? n) false
        :else (recur (dec m) (dec n))))

(deftest test-==
  (is (== two two))
  (is (not (== two one)))
  (is (not (== one two)))
  (is (== (unary 8) (inc (unary 7))))
  (is (== one one))
  (is (== two two))
  (is (not (== one (inc one))))
  (is (== two (inc one)))
  (is (== (inc zero) (dec two))))

(defn < [m n]
  (cond (zero? m) (not (zero? n))
        (zero? n) false
        :else (recur (dec m) (dec n))))

;;;
;;;    Simpler!
;;;    
(defn < [m n]
  (cond (zero? n) false ; Nothing is less than zero.
        (zero? m) true  ; Zero is less than anything but zero.
        :else (recur (dec m) (dec n))))

(deftest test-<
  (is (< zero one))
  (is (< one two))
  (is (< two (inc two)))
  (is (not (< zero zero)))
  (is (not (< one one)))
  (is (not (< one zero)))
  (is (not (< two one))))

(defn > [m n]
  (cond (zero? m) false ; Zero is not greater than anything.
        (zero? n) true  ; Anything besides zero is greater than zero.
        :else (recur (dec m) (dec n))))

(deftest test->
  (is (> two one))
  (is (> one zero))
  (is (not (> one one)))
  (is (not (> zero two)))
  (is (> (dec two) zero)))

(defn + [m n]
  (if (zero? n)
    m
    (recur (inc m) (dec n))))

(deftest test-+
  (is (== (+ one one) two))
  (is (== (+ one two) (+ two one)))
  (is (== (+ zero zero) zero))
  (is (== (+ zero one) one))
  (is (== (+ one zero) one))
  (is (== (+ two one) (inc two))))

;; (defn - [m n]
;;   (cond (zero? n) m
;;         (zero? m) (throw (IllegalArgumentException. (cl-format nil "Cannot subtract ~A" n)))
;;         :else (recur (dec m) (dec n))))

(defn - [m n]
  (letfn [(subtract [a b]
            (cond (zero? b) a
                  (zero? a) (throw (IllegalArgumentException. (cl-format nil "Cannot subtract ~A from ~A" n m)))
                  :else (recur (dec a) (dec b))))]
    (subtract m n)))

(deftest test--
  (is (== (- zero zero) zero))
  (is (== (- one zero) one))
  (is (== (- one one) zero))
  (is (== (- two two) zero))
  (is (== (- two one) one))
  (is (== (- two zero) two)))

;;;
;;;    From Lisp version
;;;    m * n = m * n + m - m = m * (n - 1) + m
;;;
(defn * [m n]
  (if (zero? n)
      zero
      (+ m (* m (dec n)))) )

(defn * [m n]
  (letfn [(multiply [n result]
            (if (zero? n)
              result
              (recur (dec n) (+ result m))))]
    (multiply n zero)))

(deftest test-*
  (is (== (* (unary 2) (unary 3)) (unary 6)))
  (is (== (* two two) (+ two two)))
  (is (== (* one (unary 8)) (unary 8)))
  (is (== (* two zero) zero))
  (is (== (* zero two) zero)))

;;;
;;;    From Lisp version
;;;    m   m - n + n    m - n
;;;    - = --------- = ------ + 1
;;;    n       n         n
;;;    
(defn / [m n]
  (cond (zero? n) (throw (IllegalArgumentException. (cl-format nil "Cannot divide by zero.")))
        (< m n) zero
        :else (inc (/ (- m n) n))))

(defn / [m n]
  (letfn [(div [m acc]
            (if (< m n)
	      acc
              (recur (- m n) (inc acc))))]
    (if (zero? n)
      (throw (IllegalArgumentException. (cl-format nil "Cannot divide by zero.")))
      (div m zero))))

(deftest test-div
  (is (== (/ (unary 4) two) two))
  (is (== (/ (unary 5) two) two))
  (is (== (/ (unary 6) two) (unary 3)))
  (is (== (/ (* two two) (* two two)) one))
  (is (== (/ zero one) zero)))

(defn mod [m n]
  (if (< m n) ; (zero? n) !!
    m
    (recur (- m n) n)))

(deftest test-mod
  (is (== (mod (unary 5) two) one))
  (is (== (mod (unary 7) two) one))
  (is (== (mod (unary 6) two) zero))
  (is (== (mod (unary 9) (unary 4)) one))
  (is (== (mod (unary 9) (unary 5)) (unary 4))))

(defn pos? [n]
  (and (unary? n) (not (zero? n))))

(deftest test-pos?
  (is (pos? one))
  (is (pos? two))
  (is (not (pos? zero)))
  (is (pos? (inc zero)))
  (is (not (pos? (- two two)))) )

(defn even? [n]
  (zero? (mod n two)))

(deftest test-even?
  (is (even? zero))
  (is (even? two))
  (is (not (even? one)))
  (is (even? (* one two))))

(defn odd? [n]
  (not (even? n)))

(deftest test-odd?
  (is (odd? one))
  (is (odd? (- two one)))
  (is (not (odd? zero)))
  (is (not (odd? two)))
  (is (odd? (* (unary 3) (unary 5)))) )


