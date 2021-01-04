;;;;
;;;;
;;;;   Clojure solves the problem that you don't know you have.
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               ch04.clj
;;;;
;;;;   Started:            Wed Dec 23 01:05:27 2020
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

(ns ch04
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    4.1
;;;    
(defn make-even [n]
  (if (odd? n)
    (inc n)
    n))

(deftest test-make-even
  (is (== (make-even 0) 0))
  (is (== (make-even 1) 2))
  (is (== (make-even 2) 2))
  (is (== (make-even -1) 0))
  (is (== (make-even -2) -2)))

;;;
;;;    4.2
;;;
(defn further [x]
  (if (pos? x)
    (inc x)
    (if (neg? x)
      (dec x)
      x)))

(deftest test-further
  (is (== (further 2) 3))
  (is (== (further -1) -2))
  (is (== (further 0) 0)))

;;;
;;;    4.3
;;;
(defn not! [expr]
  (if expr
    false
    true))

(deftest test-not
  (is (every? (fn [expr] (= (not expr) (not! expr))) '(true false 1 nil "pung" :a))))

;;;
;;;    4.4
;;;
(defn ordered [a b]
  (if (< a b)
    (list a b)
    (list b a)))

(deftest test-ordered
  (is (= (ordered 2 3) '(2 3)))
  (is (= (ordered 3 2) '(2 3)))
  (is (= (ordered 2 2) '(2 2))))

;;;
;;;    4.8
;;;
(defn emphasize [s]
  (cond (= (first s) 'good) (cons 'great (rest s))
        (= (first s) 'bad) (cons 'awful (rest s))
        :else (cons 'very s)))

(deftest test-emphasize
  (is (= (emphasize '(good mystery story)) '(great mystery story)))
  (is (= (emphasize '(mediocre mystery story)) '(very mediocre mystery story)))
  (is (= (emphasize '(good day)) '(great day)))
  (is (= (emphasize '(bad day)) '(awful day)))
  (is (= (emphasize '(long day)) '(very long day)))
  (is (= (emphasize '(very long day)) '(very very long day))))

;;;
;;;    4.10
;;;
(defn constrain [x min max]
  (cond (< x min) min
        (> x max) max
        :else x))

(defn constrain [x min max]
  (if (< x min)
    min
    (if (> x max)
      max
      x)))

(deftest test-constrain
  (is (== (constrain 3 -50 50) 3))
  (is (== (constrain 92 -50 50) 50))
  (is (== (constrain -75 -50 50) -50)))

;;;
;;;    4.11
;;;    
(defn first-zero [l]
  (cond (zero? (first l)) 'first
        (zero? (second l)) 'second
        (zero? (second (rest l))) 'third
        :else 'none))

(defn first-zero [[a b c]]
  (cond (zero? a) 'first
        (zero? b) 'second
        (zero? c) 'third
        :else 'none))

(defn first-zero [l]
  (letfn [(find-first-zero [l labels]
            (cond (empty? l) 'none
                  (zero? (first l)) (first labels)
                  :else (recur (rest l) (rest labels))))]
    (find-first-zero l '(first second third))))

(defn position [obj seq]
  (letfn [(find-it [i seq]
            (cond (empty? seq) nil
                  (= obj (first seq)) i
                  :else (recur (inc i) (rest seq))))]
    (find-it 0 seq)))

(defn first-zero [ns]
  (symbol (clojure.string/lower-case (cl-format nil "~:@(~:[none~;~:*~:R~]~)" (position 0 (cons :pung ns)))) )) ; !!!!!!!!!

(defn first-zero [ns]
  (case (position 0 ns)
    0 'first
    1 'second
    2 'third
    'none))

(deftest test-first-zero
  (is (= (first-zero '(0 3 4)) 'first))
  (is (= (first-zero '(3 0 4)) 'second))
  (is (= (first-zero '(3 4 0)) 'third))
  (is (= (first-zero '(3 2 4)) 'none)))

;;;
;;;    4.12
;;;    
(def ^:const cycle-length 99)
(defn cycle [i]
  (cond (== i cycle-length) 1
        :else (inc i)))

(defn cycle [i]
  (if (>= i cycle-length)
    (recur (mod i cycle-length))
    (inc i)))

(defn cycle [i]
  (inc (mod i cycle-length)))

(deftest test-cycle
  (is (== (cycle 1) 2))
  (is (== (cycle 2) 3))
  (is (== (cycle 98) 99))
  (is (== (cycle cycle-length) 1)))
  
;;;
;;;    4.13
;;;
(defn safecall [f op1 op2 result]
  (try (== (f op1 op2) result)
       (catch ArithmeticException e false)))

(defn how-compute [op1 op2 result]
  (let [op-map {+ 'sum-of - 'difference-of * 'product-of / 'quotient-of}
        operations (filter (fn [f] (safecall f op1 op2 result)) (list + - * /))]
    (if (empty? operations)
      '(beats me)
      (map (fn [f] (op-map f)) operations))))

(deftest test-how-compute
  (is (= (how-compute 3 4 7) '(sum-of)))
  (is (= (how-compute 3 4 12) '(product-of)))
  (is (= (how-compute 9 3 6) '(difference-of)))
  (is (= (how-compute 12 3 4) '(quotient-of)))
  (is (= (how-compute 4 2 2) '(difference-of quotient-of)))
  (is (= (how-compute 1 2 9) '(beats me)))
  (is (= (how-compute 2 2 4) '(sum-of product-of)))
  (is (= (how-compute 0 0 0) '(sum-of difference-of product-of))))

;;;
;;;    4.15
;;;    
(defn geq [x y]
  (or (== x y)
      (> x y)))

(def geq >=)
  
(deftest test-geq
  (is (geq 3 3))
  (is (geq 3 2))
  (is (clojure.core/not (geq 2 3))))

;;;
;;;    4.16
;;;    
(defn fancy [n]
  (cond (and (odd? n) (pos? n)) (* n n)
        (and (odd? n) (neg? n)) (* 2 n)
        :else (/ n 2)))

(defn fancy-factor [n]
  (if (odd? n)
    (if (pos? n) n 2)
    1/2))

(defn fancy [n]
  (* n (fancy-factor n)))

(deftest test-fancy
  (is (== (fancy 3) 9))
  (is (== (fancy -7) -14))
  (is (== (fancy 0) 0))
  (is (== (fancy 8) 4))
  (is (== (fancy -4) -2)))

;;;
;;;    4.17
;;;
(defn categorize [sex age]
  (case age
    child (contains? #{'boy 'girl} sex)
    adult (contains? #{'man 'woman} sex)))

(defn categorize [sex age]
  (case sex
    (boy girl) (= age 'child)
    (man woman) (= age 'adult)))

(deftest test-categorize
  (is (categorize 'boy 'child))
  (is (categorize 'girl 'child))
  (is (categorize 'man 'adult))
  (is (categorize 'woman 'adult))
  (is (clojure.core/not (categorize 'boy 'adult)))
  (is (clojure.core/not (categorize 'girl 'adult)))
  (is (clojure.core/not (categorize 'man 'child)))
  (is (clojure.core/not (categorize 'woman 'child))))

;;;
;;;    4.18
;;;    
(def jan-ken-pon {:rock [:scissors :paper]
                  :paper [:rock :scissors]
                  :scissors [:paper :rock]})

(defn play [first second]
  (let [results (jan-ken-pon first)]
    (if (empty? results)
      (throw (IllegalArgumentException. (cl-format nil "Invalid move ~A." first)))
      (let [[win lose] results]
        (cond (= first second) 'tie
              (= second win) 'first-wins
              (= second lose) 'second-wins
              :else (throw (IllegalArgumentException. (cl-format nil "Invalid move ~A." second)))) ))))

(defn play [first second]
  (letfn [(wins? [a b]
            (case a
              :rock (= b :scissors)
              :paper (= b :rock)
              :scissors (= b :paper)
              (throw (IllegalArgumentException. (cl-format nil "Invalid move ~A." a)))) )]
    (cond (wins? first second) 'first-wins
          (wins? second first) 'second-wins
          :else 'tie)))

(deftest test-play
  (is (= (play :rock :rock) 'tie))
  (is (= (play :rock :paper) 'second-wins))
  (is (= (play :rock :scissors) 'first-wins))
  (is (= (play :paper :rock) 'first-wins))
  (is (= (play :paper :paper) 'tie))
  (is (= (play :paper :scissors) 'second-wins))
  (is (= (play :scissors :rock) 'second-wins))
  (is (= (play :scissors :paper) 'first-wins))
  (is (= (play :scissors :scissors) 'tie)))

;;;
;;;    4.20
;;;
;;;    Touretzky pg. 116 (No default clause)
;;;    
(defn compare [x y]
  (cond (== x y) 'numbers-are-the-same
        (< x y) 'first-is-smaller
        (> x y) 'first-is-bigger))

(defn compare [x y]
  (case (Math/signum (double (clojure.core/compare x y))) ; Yuck...
    0.0 'numbers-are-the-same
    1.0 'first-is-bigger
    -1.0 'first-is-smaller))

(defn compare [x y]
  (if (< x y)
    'first-is-smaller
    (if (> x y)
      'first-is-bigger
      'numbers-are-the-same)))

(defn compare [x y]
  (or (and (< x y) 'first-is-smaller)
      (and (> x y) 'first-is-bigger)
      'numbers-are-the-same))

(deftest test-compare
  (is (= (compare 3 5) 'first-is-smaller))
  (is (= (compare 7 2) 'first-is-bigger))
  (is (= (compare 4 4) 'numbers-are-the-same))
  (is (= (compare 9 1) 'first-is-bigger))
  (is (= (compare (* 2 2) 5) 'first-is-smaller))
  (is (= (compare 6 (* 2 3)) 'numbers-are-the-same)))

;;;
;;;    4.21
;;;
(defn gtest [x y]
  (or (> x y)
      (zero? x)
      (zero? y)))

(defn gtest [x y]
  (if (> x y)
      true
      (if (zero? x)
          true
          (zero? y))))

;;;
;;;    Verbose...
;;;    
(defn gtest [x y]
  (cond (> x y) true
        (zero? x) true
        (zero? y) true
        :else false))

(deftest test-gtest
  (is (gtest 9 4))
  (is (gtest 9.0 4.0))
  (is (gtest 9 4.0))
  (is (not (gtest 4 9)))
  (is (gtest 9 0))
  (is (gtest 0 4))
  (is (gtest 0.0 0.0)))

;;;
;;;    4.22
;;;
(defn boiling? [temperature scale]
  (cond (= scale :fahrenheit) (> temperature 212)
        (= scale :celsius) (> temperature 100)
        :else (throw (IllegalArgumentException. (cl-format nil "Invalid temperature scale ~A." scale)))) )

(defn boiling? [temperature scale]
  (case scale
    :fahrenheit (> temperature 212)
    :celsius (> temperature 100)
    (throw (IllegalArgumentException. (cl-format nil "Invalid temperature scale ~A." scale)))) )

(def boiling-points {:fahrenheit 212 :celsius 100})

(defn boiling? [temperature scale]
  (let [boiling-point (boiling-points scale)]
    (if (nil? boiling-point)
      (throw (IllegalArgumentException. (cl-format nil "Invalid temperature scale ~A." scale)))
      (> temperature boiling-point))))

(defn boiling? [temperature scale]
  (if (= scale :fahrenheit) 
    (> temperature 212)
    (if (= scale :celsius)
      (> temperature 100)
      (throw (IllegalArgumentException. (cl-format nil "Invalid temperature scale ~A." scale)))) ))

(defn boiling? [temperature scale]
  (or (and (= scale :fahrenheit) (> temperature 212))
      (and (= scale :celsius) (> temperature 100))))

(deftest test-boiling?
  (is (boiling? 270 :fahrenheit))
  (is (boiling? 212.1 :fahrenheit))
  (is (not (boiling? 200 :fahrenheit)))
  (is (not (boiling? 32 :fahrenheit)))
  (is (boiling? 115 :celsius))
  (is (boiling? 100.1 :celsius))
  (is (not (boiling? 99 :celsius)))
  (is (not (boiling? -40 :celsius))))

