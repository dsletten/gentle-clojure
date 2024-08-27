;;;;
;;;;
;;;;   One of the nice things about Clojure is that it lets you fix Java
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               unary_test.clj
;;;;
;;;;   Started:            Mon May 20 23:23:24 2024
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
;;;;    lein test touretzky.unary-test                                                                                                
;;;;    {:result true, :num-tests 200, :seed 1721422622725, :time-elapsed-ms 257, :test-var "commutative-addition"}                   
;;;;    {:result true, :num-tests 200, :seed 1721422622987, :time-elapsed-ms 72, :test-var "strict-total-order"}                      
;;;;    {:result true, :num-tests 200, :seed 1721422623059, :time-elapsed-ms 71550, :test-var "distributive-multiplication-addition"} 
;;;;    {:result true, :num-tests 200, :seed 1721422694616, :time-elapsed-ms 18547, :test-var "commutative-multiplication"}           
;;;;    {:result true, :num-tests 200, :seed 1721422713167, :time-elapsed-ms 791, :test-var "associative-addition"}                   
;;;;    {:result true, :num-tests 200, :seed 1721422713959, :time-elapsed-ms 10954751, :test-var "associative-multiplication"}        
;;;;
;;;;    3 hours!!!
;;;;    (floor 10954751 (* 60 1000))
;;;;    182                         
;;;;    34751                       

(ns touretzky.unary-test
  (:require [clojure.spec.alpha :as s]
;            [clojure.spec.test.alpha :as st]
            [clojure.test :refer [are deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [touretzky.unary :as u]))

(st/instrument)

(deftest test-zero?
  (is (u/zero? u/zero))
  (is (not (u/zero? u/one)))
  (is (not (u/zero? (u/inc u/zero))))
  (is (u/zero? (u/dec u/one))))

(deftest test-inc
  (is (u/== u/one (u/inc u/zero)))
  (is (u/> (u/inc u/one) u/one)))

(deftest test-dec
  (is (thrown-with-msg? IllegalArgumentException
                        #"Cannot decrement zero."
                        (u/dec u/zero)
                        "Homie don't play with negative values."))
  (is (u/== u/zero (u/dec u/one)))
  (is (u/< (u/dec u/one) u/one)))

(deftest test-=
  (is (u/== u/one u/one u/one u/one u/one u/one u/one u/one u/one u/one u/one u/one u/one)))

(deftest test-<
  (let [two   (u/+ u/one u/one)
        three (u/+ two u/one)
        four  (u/+ two two)
        five  (u/+ two three)]
    (is (not (u/< two three three four)))) )
  
(deftest test->=
  (let [two   (u/+ u/one u/one)
        three (u/+ two u/one)
        four  (u/+ two two)
        five  (u/+ two three)]
    (is (u/>= u/one u/zero))
    (is (u/>= u/one u/one))
    (is (u/>= u/one u/one u/one))
    (is (not (u/>= three two three)))
    (is (u/>= three three two))
    (is (u/>= five four three two u/one u/zero))
    (is (u/>= five five four four three three two two))))
  
(deftest test-<=
  (let [two   (u/+ u/one u/one)
        three (u/+ two u/one)
        four  (u/+ two two)
        five  (u/+ two three)]
    (is (u/<= u/zero u/one))
    (is (u/<= u/one u/one))
    (is (u/<= u/one u/one u/one))
    (is (u/<= two three three))
    (is (u/<= u/zero u/one two three four five))
    (is (u/<= two two three three four four five five))))
  
(deftest test-+
  (let [two   (u/+ u/one u/one)
        three (u/+ two u/one)
        four  (u/+ two two)
        five  (u/+ two three)]
    (is (u/zero? (u/+)))
    (are [arg]
      (is (u/== arg (u/+ arg)))

      u/zero
      u/one
      two
      three)
    (are [expected m n]
      (is (u/== expected (u/+ m n)))

      u/zero u/zero u/zero
      u/one  u/zero u/one
      u/one  u/one  u/zero
      two    u/zero two
      two    two    u/zero
      four   u/one  three
      four   three  u/one
      five   two    three
      five   three  two
      five   four   u/one
      five   u/one  four)
    (is (u/== five
              (u/+ two two u/one)
              (u/+ u/one u/one three)
              (u/+ (u/+ u/one u/one u/one) u/one u/one)))) )

(deftest test--
  (let [two   (u/+ u/one u/one)
        three (u/+ two u/one)
        four  (u/+ two two)
        five  (u/+ two three)]
    (is (thrown-with-msg? IllegalArgumentException
                          #"Cannot subtract"
                          (u/- u/zero u/one)
                          "Homie don't play with negative values."))
    (is (thrown-with-msg? IllegalArgumentException
                          #"Cannot subtract"
                          (u/- five three three)
                          "Homie don't play with negative values."))
    (are [expected m n]
      (is (u/== expected (u/- m n)))

      u/zero u/zero u/zero
      u/zero u/one  u/one
      u/zero two    two
      u/zero five   five
      u/one  u/one  u/zero
      two    two    u/zero
      three  four   u/one
      u/one  four   three
      three  five   two    
      two    five   three
      u/one  five   four
      four   five   u/one)
    (is (u/== u/zero
              (u/- two u/one u/one)
              (u/- three two u/one)
              (u/- (u/- four u/one u/one) u/one u/one)))) )
      
(deftest test-*
  (let [two   (u/+ u/one u/one)
        three (u/+ two u/one)
        four  (u/+ two two)
        five  (u/+ two three)
        eight (u/+ four four)
        ten   (u/+ five five)]
    (is (u/== u/one (u/*)))
    (are [arg]
      (is (u/== arg (u/* arg)))

      u/zero
      u/one
      two
      three)
    (are [expected m n]
      (is (u/== expected (u/* m n)))

      u/zero u/zero u/zero
      u/zero u/zero u/one
      u/zero u/one  u/zero
      u/zero u/zero two
      two    two    u/one
      four   four   u/one
      five   five   u/one
      three  u/one  three
      three  three  u/one
      eight  two    four
      eight  four   two
      ten    five   two
      ten    two   five)
    (is (u/== eight
              (u/* two two two)
              (u/* u/one two four)
              (u/* (u/* two u/one two) u/one two)))) )

(defspec commutative-addition 200
  (prop/for-all [m (s/gen ::u/number)
                 n (s/gen ::u/number)]
    (is (u/== (u/+ m n) (u/+ n m)))) )

(defspec associative-addition 200
  (prop/for-all [a (s/gen ::u/number)
                 b (s/gen ::u/number)
                 c (s/gen ::u/number)]
    (is (u/== (u/+ a (u/+ b c)) (u/+ (u/+ a b) c) (u/+ a b c)))) )

(defspec commutative-multiplication 50
  (prop/for-all [m (s/gen ::u/number)
                 n (s/gen ::u/number)]
    (is (u/== (u/* m n) (u/* n m)))) )

(defspec associative-multiplication 5 ; 200
  (prop/for-all [a (s/gen ::u/number)
                 b (s/gen ::u/number)
                 c (s/gen ::u/number)]
    (is (u/== (u/* a (u/* b c)) (u/* (u/* a b) c) (u/* a b c)))) )

(defspec distributive-multiplication-addition 50
  (prop/for-all [a (s/gen ::u/number)
                 b (s/gen ::u/number)
                 c (s/gen ::u/number)]
    (is (u/== (u/* a (u/+ b c)) (u/+ (u/* a b) (u/* a c)))) ))

(defspec strict-total-order 200
  (prop/for-all [m (s/gen ::u/number)
                 n (s/gen ::u/number)]
    (is (or (u/< m n) (u/> m n) (u/== m n)))) )

(comment
(let [two (+ one one) three (+ two one)] (> two three))
(let [two (+ one one) three (+ two one)] (> three two))
(let [two (+ one one) three (+ two one)] (> three two one))
(let [two (+ one one) three (+ two one)] (> three two one zero))

(let [two (+ one one) three (+ two one)] (< two three))
(let [two (+ one one) three (+ two one)] (< one two three))

(let [two (+ one one) three (+ two one)] (* two three))
(let [two (+ one one) three (+ two one)] (* two zero))
(let [two (+ one one) three (+ two one)] (* zero two))
(let [two (+ one one) three (+ two one)] (* two two two))

)


