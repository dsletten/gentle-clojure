;;;;
;;;;
;;;;   One of the nice things about Clojure is that it lets you fix Java
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               ch04_test.clj
;;;;
;;;;   Started:            Wed Nov 13 21:49:06 2024
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

(ns touretzky.ch04-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [are deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [touretzky.ch04 :as ch04]))

(st/instrument)

(deftest test-make-even
  (is (== 0 (ch04/make-even 0)))
  (is (== 2 (ch04/make-even 1)))
  (is (== 2 (ch04/make-even 2)))
  (is (== 0 (ch04/make-even -1)))
  (is (== -2 (ch04/make-even -2))))

(deftest test-further
  (is (== 2 (ch04/further 1)))
  (is (== 1.5 (ch04/further 0.5)))
  (is (== 5/2 (ch04/further 3/2)))
  (is (== -2 (ch04/further -1)))
  (is (== -1.5 (ch04/further -0.5)))
  (is (== -5/2 (ch04/further -3/2)))
  (is (== 0 (ch04/further 0)))
  (is (== 0.0 (ch04/further 0.0))))
  
(deftest test-ordered
  (is (= '(2 3) (ch04/ordered 2 3)))
  (is (= '(2 3) (ch04/ordered 3 2)))
  (is (= '(2 2) (ch04/ordered 2 2))))

(deftest test-compute
  (is (== 10 (ch04/compute 'sum-of 3 7)))
  (is (== 8  (ch04/compute 'product-of 2 4)))
  (is (= '(that does not compute) (ch04/compute 'zorch-of 3 1))))

(deftest test-emphasize
  (is (= '(great day) (ch04/emphasize '(good day))))
  (is (= '(awful day) (ch04/emphasize '(bad day))))
  (is (= '(very long day) (ch04/emphasize '(long day))))
  (is (= '(very very long day) (ch04/emphasize (ch04/emphasize '(long day)))) ))

(deftest test-make-odd
  (is (odd? (ch04/make-odd -2)))
  (is (odd? (ch04/make-odd -1)))
  (is (odd? (ch04/make-odd 0)))
  (is (odd? (ch04/make-odd 1)))
  (is (odd? (ch04/make-odd 2))))

(deftest test-constrain
  (is (== 3 (ch04/constrain 3 -50 50)))
  (is (== 50 (ch04/constrain 92 -50 50)))
  (is (== 0 (ch04/constrain -1 0 10))))

(deftest test-first-zero
  (is (= 'first (ch04/first-zero '(0 3 4))))
  (is (= 'first (ch04/first-zero '(0 0 4))))
  (is (= 'first (ch04/first-zero '(0 3 0))))
  (is (= 'first (ch04/first-zero '(0 0 0))))
  (is (= 'second (ch04/first-zero '(3 0 4))))
  (is (= 'second (ch04/first-zero '(3 0 0))))
  (is (= 'third (ch04/first-zero '(3 4 0))))
  (is (= 'none (ch04/first-zero '(1 2 3))))
  (is (thrown-with-msg? IllegalArgumentException
                        #"Bad input"
                        (ch04/first-zero '(1 2 3 0)))) )
;  (is (= 'none (ch04/first-zero '(1 2 3 0)))) )

(deftest test-cycle
  (is (every? true? (map #(== (inc %1) %2) (range 1 99) (map #(ch04/cycle % 99) (range 1 99)))) )
  (is (== 1 (ch04/cycle 99 99))))

(deftest test-how-alike
  (is (= 'the-same (ch04/how-alike 2 2)))
  (is (= 'both-odd (ch04/how-alike 3 5)))
  (is (= 'both-even (ch04/how-alike 2 8)))
  (is (= 'both-negative (ch04/how-alike -3 -4)))
  (is (= 'both-positive (ch04/how-alike 3 12)))
  (is (= 'the-same (ch04/how-alike 0 0)))
  (is (= 'not-alike (ch04/how-alike 3 0))))

(deftest test-same-sign?
  (is (ch04/same-sign? 0 0))
  (is (ch04/same-sign? 0 0.0))
  (is (ch04/same-sign? 0.0 -0.0))
  (is (ch04/same-sign? 3 4))
  (is (ch04/same-sign? -3 -4))
  (is (not (ch04/same-sign? 3 -4))))

(defspec test-geq 400
  (prop/for-all [m (s/gen number?)
                 n (s/gen number?)]
    (is (= (>= m n) (ch04/geq m n)))))

(deftest test-fancy
  (is (== (ch04/fancy 3) 9))
  (is (== (ch04/fancy -7) -14))
  (is (== (ch04/fancy 0) 0))
  (is (== (ch04/fancy 8) 4))
  (is (== (ch04/fancy -4) -2)))

(deftest test-categorize
  (is (ch04/categorize 'boy 'child))
  (is (ch04/categorize 'girl 'child))
  (is (ch04/categorize 'man 'adult))
  (is (ch04/categorize 'woman 'adult))
  (is (not (ch04/categorize 'boy 'adult)))
  (is (not (ch04/categorize 'girl 'adult)))
  (is (not (ch04/categorize 'man 'child)))
  (is (not (ch04/categorize 'woman 'child))))

(deftest test-play
  (is (= 'tie (ch04/play 'rock 'rock)))
  (is (= 'second-player (ch04/play 'rock 'paper)))
  (is (= 'first-player (ch04/play 'rock 'scissors)))
  (is (= 'first-player (ch04/play 'paper 'rock)))
  (is (= 'tie (ch04/play 'paper 'paper)))
  (is (= 'second-player (ch04/play 'paper 'scissors)))
  (is (= 'second-player (ch04/play 'scissors 'rock)))
  (is (= 'first-player (ch04/play 'scissors 'paper)))
  (is (= 'tie (ch04/play 'scissors 'scissors))))
