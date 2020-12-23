;;;;
;;;;
;;;;   In Clojure, because the language is so bendable, you actually bend language towards the problem, not the problem towards the language.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               ch03.clj
;;;;
;;;;   Started:            Sat Dec 19 03:23:08 2020
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
        [clojure.pprint :only (cl-format)])
  (:import))

;; (defn abs "(abs x) is the absolute value of x" [x]
;;   (cond (not (number? x)) (throw (IllegalArgumentException. "abs requires a number"))
;;         (neg? x) (- x)
;;         :else x))

(defn approximately=
  ([a b] (approximately= a b 1e-6))
  ([a b epsilon] (<= (abs (- a b)) (* epsilon (abs a)))) )

;;;
;;;    3.5
;;;    
(defn half [x]
  (/ x 2))

(deftest test-half
  (is (== (half 4) 2))
  (is (== (half 4.0) 2.0))
  (is (== (half 3) 3/2))
  (is (zero? (half 0))))

(defn cube [x]
  (* x x x))

(deftest test-cube
  (is (== (cube 1) 1))
  (is (== (cube 2) 8))
  (is (== (cube 3) 27))
  (is (== (cube -2) -8))
  (is (== (cube -3) -27))
  (is (approximately= (cube Math/PI) (Math/pow Math/PI 3))))

(defn one-more? [x y]
  (== x (inc y)))

(deftest test-one-more?
  (is (one-more? 3 2))
  (is (one-more? -2 -3))
  (is (one-more? 5/2 3/2))
  (is (not (one-more? 2 3)))
  (is (not (one-more? 2 2))))

;;;
;;;    3.6
;;;
(defn square [x]
  (* x x))

(defn pythag [a b]
;  (Math/sqrt (+ (square a) (square b))))
  (sqrt (+ (square a) (square b))))

(deftest test-pythag
  (is (== (pythag 3 4) 5))
  (is (== (pythag 5 12) 13))
  (is (== (pythag 3.0 4.0) 5.0))
  (is (== (pythag 5.0 12.0) 13.0)))

;;;
;;;    3.7
;;;
(defn miles-per-gallon [initial-odometer-reading final-odometer-reading gallons-consumed]
  (/ (- final-odometer-reading initial-odometer-reading) gallons-consumed))

(deftest test-miles-per-gallon
  (is (== (miles-per-gallon 0 420 18) 70/3))
  (is (== (miles-per-gallon 0 420.3 18) 23.35)))

;;;
;;;    3.11
;;;
(defn longer-than [l1 l2]
  (cond (empty? l1) false
        (empty? l2) true
        :else (recur (rest l1) (rest l2))))

;;;
;;;    Compare unary.clj
;;;    
;; (defn > [m n]
;;   (cond (zero? m) false ; Zero is not greater than anything.
;;         (zero? n) true  ; Anything besides zero is greater than zero.
;;         :else (recur (dec m) (dec n))))

(defn longer-than [l1 l2]
  (> (count l1) (count l2)))

(deftest test-longer-than
  (is (longer-than '(a b c) '(a b)))
  (is (longer-than '(a) '()))
  (is (not (longer-than '(a b) '(a b c))))
  (is (not (longer-than '(a b c) '(a b c)))) )

;;;
;;;    3.12
;;;    
(defn add-length [l]
  (cons (count l) l))

(defn add-length [l]
  (letfn [(length [l result]
            (if (empty? l)
              result
              (recur (rest l) (inc result))))]
    (cons (length l 0) l)))

;;;
;;;    This is crazy...
;;;    
(defn add-length [l]
  (if (empty? l)
      (list 0)
      (let [[length _] (add-length (rest l))]
        (cons (inc length) l))))

(deftest test-add-length
  (is (= (add-length '()) '(0)))
  (is (= (add-length (add-length '())) '(1 0)))
  (is (= (add-length '(a b c)) '(3 a b c)))
  (is (= (add-length '(moo goo gai pan)) '(4 moo goo gai pan))) ; Case-sensitive!!
  (is (= (add-length (add-length '(a b c))) '(4 3 a b c))))

;;;
;;;    3.22. e.
;;;
(defn mid-add1 [l]
  (list (nth l 0) (inc (nth l 1)) (nth l 2)))

(defn mid-add1 [[a b c]]
  (list a (inc b) c))

(deftest test-mid-add1
  (is (= (mid-add1 '(take 2 cookies)) '(take 3 cookies)))
  (is (= (mid-add1 '[take 2 cookies]) '(take 3 cookies))) ; !!!
  (is (= (mid-add1 '[take 2 cookies]) '[take 3 cookies])))

;;;
;;;    3.22. f.
;;;    
(defn f-to-c [f]
  (* 5/9 (- f 32.0)))

(deftest test-f-to-c
  (is (== (f-to-c 32) 0))
  (is (== (f-to-c 98.6) 37))
  (is (== (f-to-c 212) 100)))

(defn convert [scale temp]
  (- (* scale (+ temp 40)) 40))

(defn f->c [f]
  (convert 5/9 f))

(defn c->f [c]
  (convert 9/5 c))

(deftest test-f->c
  (is (== (f->c 32) 0))
  (is (== (f->c 98.6) 37))
  (is (== (f->c 212) 100)))

(deftest test-c->f
  (is (== (c->f 0) 32))
  (is (== (c->f 37.0) 98.6))
  (is (== (c->f 100) 212))
  (is (== (f->c (c->f 80)) 80)))

