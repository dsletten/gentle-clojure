;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch03.clj
;;;;
;;;;   Started:            Mon Sep  2 01:36:33 2019
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
;;;    3.5 200404
;;;    
(defn half [x]
  (/ x 2))

(deftest test-half
  (is (== (half 8) 4))
  (is (== (half 0) 0))
  (is (== (half 3) 3/2))
  (is (== (half 3.0) 1.5)))

(defn cube [x]
  (* x x x ))

(deftest test-cube
  (is (== (cube 5) 125))
  (is (== (cube 3.0) 27.0))
  (is (== (cube -2) -8))
  (is (zero? (cube 0)))
  (is (== (cube 2) 8))
  (is (== (cube 0) 0))
  (is (== (cube -3) -27))
  (is (== (cube Math/PI) 31.006276680299816)))
;   (approximately= (cube pi) 31.006276680299816D0)))
  
(defn approximately= 
  ([a b] (approximately= a b 1e-6))
  ([a b epsilon]
     (<= (abs (- a b)) (* epsilon (abs a)))) )

(deftest test-approximately=
  (is (approximately= 0.001 0.0010000002))
  (is (not (approximately= 0.001 0.001000002)))
  (is (approximately= 0.001 0.001000002 1e-4)))

(defn onemore? [x y]
  (== x (inc y)))

(deftest test-onemore?
  (is (onemore? 3 2))
  (is (onemore? 3.1 2.1))
  (is (onemore? 3.1 2.1))
  (is (not (onemore? 0 5)))
  (is (onemore? 9 8))
  (is (onemore? 5/2 3/2))
  (is (not (onemore? 8 9)))
  (is (not (onemore? 0 0))))

;;;
;;;    3.6
;;;
(defn hypotenuse [a b]
  (sqrt (+ (* a a) (* b b))))

(deftest test-hypotenuse
  (is (== (hypotenuse 3 4) 5))
  (is (== (hypotenuse 3.0 4.0) 5.0))
  (is (== (hypotenuse 5 12) 13))
  (is (== (hypotenuse 5.0 12.0) 13.0)))

(defn pythag [& nums]
  (sqrt (reduce + (map (fn [x] (* x x)) nums))))

(deftest test-pythag
  (is (== (pythag 2) 2))
  (is (== (pythag 3 4) 5))
  (is (== (pythag 3 4 12) 13)))

;;;
;;;    3.7
;;;
(defn miles-per-gallon [initial-odometer-reading final-odometer-reading gallons-consumed]
  (/ (- final-odometer-reading initial-odometer-reading)
     gallons-consumed))

(deftest test-miles-per-gallon
  (is (== (miles-per-gallon 0 420 18) 70/3))
  (is (== (miles-per-gallon 0 420.3 18) 23.35)))

;;;
;;;    3.11
;;;
(defn longer? [l1 l2]
  (> (count l1) (count l2)))

(defn longer? [l1 l2]
  (cond (empty? l1) false
        (empty? l2) true
        :else (recur (rest l1) (rest l2))))
;        :else (longer? (rest l1) (rest l2))))

(deftest test-longer?
  (is (longer? '(a b c) '(a b)))
  (is (longer? '(a) '()))
  (is (not (longer? '(a b) '(a b c))))
  (is (not (longer? '(a b c) '(a b c)))) )

;;;
;;;    3.12
;;;
(defn add-length [l]
  (cons (count l) l))

(defn add-length [l]
  (cond (empty? l) '(0)
        :else (let [[n & _] (add-length (rest l))]
                (cons (inc n) l))))

(defn add-length [l]
  (letfn [(length [l len]
            (if (empty? l)
              len
              (recur (rest l) (inc len))))]
    (cons (length l 0) l)))

(deftest test-add-length
  (is (= (add-length '()) '(0)))
  (is (= (add-length (add-length '())) '(1 0)))
  (is (= (add-length '(moo goo gai pan)) '(4 moo goo gai pan)))
  (is (= (add-length (add-length '(a b c))) '(4 3 a b c))))

;;;
;;;    3.22d
;;;    
(defn first? [sym l]
  (= sym (first l)))

(deftest test-first?
  (is (first? 'foo '(foo bar baz)))
  (is (not (first? 'boing '(foo bar baz)))) )

;;;
;;;    3.22e
;;;    
(defn mid-add1 [l]
  (cons (first l) (cons (inc (second l)) (nthrest l 2))))

(defn mid-add1 [l]
  (let [[a b c] l]
    (list a (inc b) c)))

(defn mid-add1 [[a b c]]
  (list a (inc b) c))

(deftest test-mid-add1
  (is (= (mid-add1 '(take 2 cookies)) '(take 3 cookies))))

;;;
;;;    3.22f
;;;
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
  (is (== (c->f (f->c 80)) 80)))





