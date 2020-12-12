;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch04.clj
;;;;
;;;;   Started:            Sat Jul 27 03:22:07 2013
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
        clojure.math.numeric-tower
        [clojure.pprint :only (cl-format)]))

;;;
;;;    4.1
;;;
(defn make-even [n]
  (if (odd? n)
    (inc n)
    n))

(deftest test-make-even
  (is (== (make-even -1) 0))
  (is (== (make-even 0) 0))
  (is (== (make-even 1) 2))
  (is (== (make-even 2) 2)))

;;;
;;;    4.2
;;;
;; (defn further [x]
;;   (if (pos? x)
;;     (inc x)
;;     (dec x)))

;; (deftest test-further
;;   (is (== (further -1) -2))
;;   (is (== (further 1) 2))
;;   (is (== (further 0) -1)))

(defn signum [x]
  (if (zero? x)
    x
    (/ (abs x) x)))

(defn further [x]
  (if (pos? x)
    (inc x)
    (if (neg? x)
      (dec x)
      x)))

(defn further [x]
  (if (zero? x)
    x
    (+ (/ (abs x) x) x)))

(defn further [x]
  (+ x (signum x)))

(defn further [x]
  (cond (pos? x) (inc x)
        (neg? x) (dec x)
        :else x))

(deftest test-further
  (is (== (further -1) -2))
  (is (== (further 1) 2))
  (is (== (further -1.5) -2.5))
  (is (== (further 1.5) 2.5))
  (is (== (further 0) 0)))

;;;
;;;    4.3
;;;
(defn not [expr]
  (if expr
    false
    true))

(deftest test-not
  (is (not false))
  (is (clojure.core/not (not true)))
  (is (identical? (not true) (clojure.core/not true)))
  (is (identical? (not false) (clojure.core/not false))))

;;;
;;;    4.4
;;;
(defn ordered [x y]
  (if (< x y)
    (list x y)
    (list y x)))

(deftest test-ordered
  (is (= (ordered 2 3) '(2 3)))
  (is (= (ordered 2 2) '(2 2)))
  (is (= (ordered 3 2) '(2 3))))

;;;
;;;    4.6
;;;
(defn my-abs [x]
  (cond (neg? x) (- x)
        :else x))

(deftest test-abs
  (is (== (my-abs -1) 1))
  (is (== (my-abs 0) 0))
  (is (== (my-abs 1) 1)))
  
;;;
;;;    4.8
;;;    
(defn emphasize [phrase]
  (cond (= (first phrase) 'good) (cons 'great (rest phrase))
        (= (first phrase) 'bad) (cons 'awful (rest phrase))
        :else (cons 'very phrase)))

(defn emphasize [[adj & more :as phrase]]
  (case adj
        good (cons 'great more)
        bad (cons 'awful more)
        (cons 'very phrase)))

(deftest test-emphasize
  (is (= (emphasize '(good mystery story)) '(great mystery story)))
  (is (= (emphasize '(bad day)) '(awful day)))
  (is (= (emphasize '(mediocre mystery story)) '(very mediocre mystery story))))

;;;
;;;    4.10
;;;
(defn constrain [x min max]
  (if (< x min)
    min
    (if (> x max)
      max
      x)))

(defn constrain [x min max]
  (cond (< x min) min
        (> x max) max
        :else x))

(deftest test-constrain
  (is (== (constrain 3 -50 50) 3))
  (is (== (constrain 92 -50 50) 50))
  (is (== (constrain -74 -50 50) -50)))

;;;
;;;    4.11
;;;
(defn firstzero [l]
  (cond (zero? (first l)) "first"
        (zero? (second l)) "second"
        (zero? (first (rest (rest l)))) "third"
        :else "none"))

(defn firstzero [l]
  (loop [l l
         labels ["first" "second" "third"]]
    (cond (or (empty? l) (empty? labels)) "none"
          (zero? (first l)) (first labels)
          :else (recur (rest l) (rest labels)))) )

(defn firstzero [[a b c]]
  (cond (zero? a) "first"
        (zero? b) "second"
        (zero? c) "third"
        :else "none"))

(defn firstzero [l]
  (loop [i 1
         l l]
    (cond (empty? l) "none"
          (zero? (first l)) (cl-format nil "~:R" i)
          :else (recur (inc i) (rest l)))) )

(deftest test-firstzero
  (is (= (firstzero '(0 1 2)) "first"))
  (is (= (firstzero '(1 0 2)) "second"))
  (is (= (firstzero '(1 2 0)) "third"))
  (is (= (firstzero '(1 2 3 4 5 6 7 8 9 0 11 12 13)) "tenth"))
  (is (= (firstzero '(1 2 3)) "none")))

;;;
;;;    4.12
;;;
(defn cycle [n]
  (if (== n 99)
    1
    (inc n)))

(defn cycle [n]
  (inc (rem n 99)))

(deftest test-cycle
  (is (== (cycle 1) 2))
  (is (== (cycle 99) 1)))

;;;
;;;    4.13
;;;
(defn howcompute [op1 op2 result]
  (cond (== result (+ op1 op2)) 'sum-of
        (== result (- op1 op2)) 'difference-of
        (== result (* op1 op2)) 'product-of
        (== result (/ op1 op2)) 'quotient-of
        :else '(beats me)))

(deftest test-howcompute
  (is (= (howcompute 3 4 7) 'sum-of))
  (is (= (howcompute 8 5 3) 'difference-of))
  (is (= (howcompute 3 4 12) 'product-of))
  (is (= (howcompute 15 5 3) 'quotient-of))
  (is (= (howcompute 3 4 9) '(beats me))))

(defn how-alike [a b]
  (cond (== a b) 'the-same
        (and (odd? a) (odd? b)) 'both-odd
        (and (even? a) (even? b)) 'both-even
        (and (neg? a) (neg? b)) 'both-negative
        (and (pos? a) (pos? b)) 'both-positive
        (and (zero? a) (zero? b)) 'both-zero
        :else 'not-alike))

(defn same-sign [a b]
  (cond (zero? a) (zero? b)
        (pos? a) (pos? b)
        (neg? a) (neg? b)
        :else false))

(defn same-sign [a b]
  (or (== a b 0)
      (pos? (* a b))))

;;;
;;;    4.15
;;;
(defn geq [x y]
  (or (> x y)
      (== x y)))

(defn geq [x y]
  (>= x y))

(defn geq [x y]
  (clojure.core/not (< x y)))

(deftest test-geq
  (is (geq 4 1))
  (is (geq 4 4))
  (is (clojure.core/not (geq 4 8))))

;;;
;;;    4.16
;;;
(defn funky [n]
  (cond (and (odd? n) (pos? n)) (* n n)
        (and (odd? n) (neg? n)) (* n 2)
        :else (/ n 2)))

(defn funky [n]
  (cond (even? n) (/ n 2)
        (pos? n) (* n n)
        (neg? n) (* n 2)))

(deftest test-funky
  (is (== (funky 5) 25))
  (is (== (funky -3) -6))
  (is (== (funky 8) 4))
  (is (== (funky 0) 0))
  (is (== (funky -6) -3)))

;;;
;;;    4.17
;;;
(defn categorize [sex age]
  (cond (or (= sex :boy) (= sex :girl)) (= age :child)
        (or (= sex :woman) (= sex :man)) (= age :adult)
        :else false))

(defn categorize [sex age]
  (case sex
        (:boy :girl) (= age :child)
        (:woman :man) (= age :adult)
        false))

(deftest test-categorize
  (is (categorize :boy :child))
  (is (categorize :girl :child))
  (is (categorize :woman :adult))
  (is (categorize :man :adult))
  (is (clojure.core/not (categorize :boy :adult)))
  (is (clojure.core/not (categorize :girl :adult)))
  (is (clojure.core/not (categorize :woman :child)))
  (is (clojure.core/not (categorize :man :child))))

;;;
;;;    4.18
;;;
(def moves #{:rock :scissors :paper})
(defn legal-move? [move]
  (contains? moves move))

(defn play [player1 player2]
  (cond (clojure.core/not (legal-move? player1)) (if (legal-move? player2)
                                      :illegal-player1
                                      :both-illegal)
        (clojure.core/not (legal-move? player2)) :illegal-player2
        (= player1 player2) :tie
        (and (= player1 :rock) (= player2 :scissors)) :first-wins
        (and (= player1 :paper) (= player2 :rock)) :first-wins
        (and (= player1 :scissors) (= player2 :paper)) :first-wins
        :else :second-wins))

(defn play [player1 player2]
  (case player1
        :rock (case player2
                    :rock :tie
                    :paper :second-wins
                    :scissors :first-wins
                    :illegal-player2)
        :paper (case player2
                    :rock :first-wins
                    :paper :tie
                    :scissors :second-wins
                    :illegal-player2)
        :scissors (case player2
                    :rock :second-wins
                    :paper :first-wins
                    :scissors :tie
                    :illegal-player2)
        (if (legal-move? player2)
          :illegal-player1
          :both-illegal)))

(defn dominates? [move1 move2]
  (loop [moves (clojure.core/cycle moves)
         seen #{}]
    (cond (contains? seen (first moves)) :illegal-move
          (= move1 (first moves)) (= move2 (second moves))
          :else (recur (rest moves) (conj seen (first moves)))) ))

(defn play [player1 player2]
  (cond (clojure.core/not (legal-move? player1)) (if (legal-move? player2)
                                      :illegal-player1
                                      :both-illegal)
        (clojure.core/not (legal-move? player2)) :illegal-player2
        (= player1 player2) :tie
        (dominates? player1 player2) :first-wins
        (dominates? player2 player1) :second-wins))

(deftest test-dominates?
  (is (dominates? :rock :scissors))
  (is (clojure.core/not (dominates? :rock :rock)))
  (is (clojure.core/not (dominates? :rock :paper)))
  (is (dominates? :scissors :paper))
  (is (clojure.core/not (dominates? :scissors :scissors)))
  (is (clojure.core/not (dominates? :scissors :rock)))
  (is (dominates? :paper :rock))
  (is (clojure.core/not (dominates? :paper :paper)))
  (is (clojure.core/not (dominates? :paper :scissors)))
  (is (= (dominates? :lizard :rock) :illegal-move)))

(deftest test-play
  (is (= (play :rock :scissors) :first-wins))
  (is (= (play :rock :paper) :second-wins))
  (is (= (play :rock :rock) :tie))
  (is (= (play :rock :lizard) :illegal-player2))
  (is (= (play :paper :rock) :first-wins))
  (is (= (play :paper :scissors) :second-wins))
  (is (= (play :paper :paper) :tie))
  (is (= (play :paper :lizard) :illegal-player2))
  (is (= (play :scissors :paper) :first-wins))
  (is (= (play :scissors :rock) :second-wins))
  (is (= (play :scissors :scissors) :tie))
  (is (= (play :scissors :lizard) :illegal-player2))
  (is (= (play :snake :scissors) :illegal-player1))
  (is (= (play :snake :paper) :illegal-player1))
  (is (= (play :snake :snake) :both-illegal))
  (is (= (play :snake :lizard) :both-illegal)))

;;;
;;;    4.19
;;;
(defn cond-and [x y z w]
  (cond (clojure.core/not x) false
        (clojure.core/not y) false
        (clojure.core/not z) false
        :else w))

(deftest test-cond-and
  (is (cond-and true true true true))
  (is (not-any? (fn [l] (apply cond-and l))
                '((true true true false)
                  (true true false true)
                  (true true false false)
                  (true false true true)
                  (true false true false)
                  (true false false true)
                  (true false false false)
                  (false true true true)
                  (false true true false)
                  (false true false true)
                  (false true false false)
                  (false false true true)
                  (false false true false)
                  (false false false true)
                  (false false false false)))) )

(defn cond-if [x y z w]
  (if x
    (if y
      (if z
        w
        false)
      false)
    false))

(deftest test-cond-if
  (is (cond-if true true true true))
  (is (not-any? (fn [l] (apply cond-if l))
                '((true true true false)
                  (true true false true)
                  (true true false false)
                  (true false true true)
                  (true false true false)
                  (true false false true)
                  (true false false false)
                  (false true true true)
                  (false true true false)
                  (false true false true)
                  (false true false false)
                  (false false true true)
                  (false false true false)
                  (false false false true)
                  (false false false false)))) )

;;;
;;;    4.20
;;;
(defn compare [x y]
  (cond (== x y) 'numbers-are-the-same
        (< x y) 'first-is-smaller
        (> x y) 'first-is-bigger))

(defn compare-if [x y]
  (if (== x y)
    'numbers-are-the-same
    (if (< x y)
      'first-is-smaller
      'first-is-bigger)))

(deftest test-compare-if
  (is (= (compare 2 2) (compare-if 2 2)))
  (is (= (compare 2 3) (compare-if 2 3)))
  (is (= (compare 3 2) (compare-if 3 2))))

(defn compare-and-or [x y]
  (or (and (== x y) 'numbers-are-the-same)
      (and (< x y) 'first-is-smaller)
      'first-is-bigger))

(deftest test-compare-and-or
  (is (= (compare 2 2) (compare-and-or 2 2)))
  (is (= (compare 2 3) (compare-and-or 2 3)))
  (is (= (compare 3 2) (compare-and-or 3 2))))

;;;
;;;    4.21
;;;
(defn gtest [x y]
  (or (> x y)
      (zero? x)
      (zero? y)))

(defn gtest [x y]
  (or (> x y) (zero? (* x y))))

(defn gtest-if [x y]
  (if (> x y)
    true
    (if (zero? x)
      true
      (zero? y))))

(deftest test-gtest-if
  (is (= (gtest 2 3) (gtest-if 2 3)))
  (is (= (gtest 3 2) (gtest-if 3 2)))
  (is (= (gtest 0 3) (gtest-if 0 3)))
  (is (= (gtest -2 0) (gtest-if -2 0))))

(defn gtest-cond [x y]
  (cond (> x y) true
        (zero? x) true
        (zero? y) true
        :else false))

(deftest test-gtest-cond
  (is (= (gtest 2 3) (gtest-cond 2 3)))
  (is (= (gtest 3 2) (gtest-cond 3 2)))
  (is (= (gtest 0 3) (gtest-cond 0 3)))
  (is (= (gtest -2 0) (gtest-cond -2 0))))

;;;
;;;    4.22
;;;
(defn boiling? [temp scale]
  (cond (= scale :fahrenheit) (> temp 212)
        (= scale :celsius) (> temp 100)
        (= scale :kelvin) (> temp 373.15)
        :else :unknown-scale))

(defn boiling? [temp scale]
  (if (= scale :fahrenheit)
    (> temp 212)
    (if (= scale :celsius)
      (> temp 100)
      (if (= scale :kelvin)
        (> temp 373.15)
        :unknown-scale))))

;;
;;    Don't need full generality here...
;;    
;; (defn boiling? [temp scale]
;;   (or (and (= scale :fahrenheit) (> temp 212))
;;       (and (not (= scale :fahrenheit)) (= scale :celsius) (> temp 100))
;;       (and (not (= scale :fahrenheit)) (not (= scale :celsius)) (= scale :kelvin) (> temp 373.15))
;;       (and (not (= scale :fahrenheit)) (not (= scale :celsius)) (not (= scale :kelvin)) :unknown-scale)))

(defn boiling? [temp scale]
  (or (and (= scale :fahrenheit) (> temp 212))
      (and (= scale :celsius) (> temp 100))
      (and (= scale :kelvin) (> temp 373.15))
      (and (clojure.core/not (= scale :fahrenheit)) (clojure.core/not (= scale :celsius)) (clojure.core/not (= scale :kelvin)) :unknown-scale)))

;; (defn boiling? [temp scale]
;;   (case scale
;;         :fahrenheit (> temp 212)
;;         :celsius (> temp 100)
;;         :kelvin (> temp 373.15)
;;         :unknown-scale))

(deftest test-boiling?
  (is (boiling? 220 :fahrenheit))
  (is (clojure.core/not (boiling? 212 :fahrenheit)))
  (is (clojure.core/not (boiling? 32 :fahrenheit)))
  (is (boiling? 120 :celsius))
  (is (clojure.core/not (boiling? 100 :celsius)))
  (is (clojure.core/not (boiling? 0 :celsius)))
  (is (boiling? 380 :kelvin))
  (is (clojure.core/not (boiling? 373.15 :kelvin)))
  (is (clojure.core/not (boiling? 273.15 :kelvin)))
  (is (= (boiling? 1000 :binary) :unknown-scale)))

;;;
;;;    4.29
;;;
(defn logical-and [x y]
  (and x y true))

(defn logical-and [x y]
  (if x
    (if y
      true
      false)
    false))

(defn logical-and [x y]
  (cond (clojure.core/not x) false
        (clojure.core/not y) false
        :else true))

(deftest test-logical-and
  (is (identical? (logical-and (> 3 2) (odd? 7)) true))
  (is (identical? (logical-and 'tweet 'woof) true))
  (is (identical? (logical-and :pung :foo) true))
  (is (identical? (logical-and (number? 'foo) (== 5 (+ 2 3))) false)))

;;;
;;;    4.30
;;;
(defn logical-or [x y]
  (or (and x true) (and y true)))

(defn logical-or [x y]
  (if x
    true
    (if y
      true
      false)))

(defn logical-or [x y]
  (cond x true
        y true
        :else false))

(deftest test-logical-or
  (is (identical? (logical-or (> 3 2) (odd? 7)) true))
  (is (identical? (logical-or 'tweet 'woof) true))
  (is (identical? (logical-or :pung :foo) true))
  (is (identical? (logical-or (number? 'foo) (== 5 (+ 2 3))) true))
  (is (identical? (logical-or (< 3 2) (even? 7)) false)))

;;;
;;;    4.37
;;;
(defn nand [x y]
  (clojure.core/not (and x y)))

(deftest test-nand
  (is (identical? (nand true true) (clojure.core/not (and true true))))
  (is (identical? (nand true false) (clojure.core/not (and true false))))
  (is (identical? (nand false true) (clojure.core/not (and false true))))
  (is (identical? (nand false false) (clojure.core/not (and false false)))) )

(defn not [x]
  (nand x x))

(defn logical-and [x y]
  (nand (nand x y) (nand x y)))

(defn logical-or [x y]
  (nand (nand x x) (nand y y)))

(defn nor [x y]
  (nand (nand (nand x x) (nand y y))
        (nand (nand x x) (nand y y))))

(deftest test-nor
  (is (identical? (nor true true) (clojure.core/not (or true true))))
  (is (identical? (nor true false) (clojure.core/not (or true false))))
  (is (identical? (nor false true) (clojure.core/not (or false true))))
  (is (identical? (nor false false) (clojure.core/not (or false false)))) )

;;;
;;;    4.38
;;;
(defn nor [x y]
  (clojure.core/not (or x y)))

(defn not [x]
  (nor x x))

(defn logical-or [x y]
  (nor (nor x y) (nor x y)))

(defn logical-and [x y]
  (nor (nor x x) (nor y y)))

(defn nand [x y]
  (nor (nor (nor x x) (nor y y))
       (nor (nor x x) (nor y y))))
