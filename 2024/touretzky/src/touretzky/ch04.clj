;;;;
;;;;
;;;;   With Clojure we found that the very very low friction to get things done enables you to do things that you'd otherwise never even consider
;;;;   -- Orestis Markou
;;;;
;;;;   Name:               ch04.clj
;;;;
;;;;   Started:            Mon Nov 11 22:54:01 2024
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

(ns touretzky.ch04
  (:require [clojure.spec.alpha :as s])
  (:refer-clojure :exclude [cycle]))

;;;
;;;    4.1
;;;
(s/fdef make-even
  :args (s/cat :n integer?)
  :ret  integer?)

(defn make-even [n]
  (if (even? n)
    n
    (inc n)))

;;;
;;;    4.2
;;;
(s/fdef further
  :args (s/cat :x number?)
  :ret  number?)

(defn further [x]
  (cond (pos? x) (inc x)
        (neg? x) (dec x)
        :else x))

;;;
;;;    4.4
;;;
(s/fdef ordered
  :args (s/cat :a number? :b number?)
  :ret (s/coll-of number?))

(defn ordered [a b]
  (if (< b a)
    (list b a)
    (list a b)))

(s/fdef compute
  :args (s/cat :op symbol? :x number? :y number?))

(defn compute [op x y]
  (case op
    sum-of (+ x y)
    product-of (* x y)
    '(that does not compute)))

;;;
;;;    4.8
;;;
(s/fdef emphasize
  :args (s/cat :s (s/coll-of symbol?))
  :ret  (s/coll-of symbol?))

(defn emphasize [[adj & more :as s]]
  (case adj
    good (cons 'great more)
    bad (cons 'awful more)
    (cons 'very s)))

;;;
;;;    4.9
;;;
(s/fdef make-odd
  :args (s/cat :n integer?)
  :ret  integer?)

(defn make-odd [n]
  (+ n (mod (inc n) 2)))

;;;
;;;    4.10
;;;
(s/fdef constrain
  :args (s/cat :x integer? :mn integer? :mx integer?)
  :ret  integer?)

(defn constrain [x min max]
  (cond (< x min) min
        (> x max) max
        :else x))

(defn constrain [x mn mx]
  (max (min x mx) mn))

;;;
;;;    4.11
;;;
(s/fdef first-zero
  :args (s/cat :ns (s/coll-of number?))
  :ret  symbol?)

(defn first-zero [[a b c]]
  (cond (zero? a) 'first
        (zero? b) 'second
        (zero? c) 'third
        :else 'none))

(defn first-zero [ns]
  (letfn [(check-it [ms tags]
            (cond (and (empty? ms) (empty? tags)) 'none
                  (or (empty? ms) (empty? tags)) (throw (IllegalArgumentException. (format "Bad input: %s" ns)))
                  (zero? (first ms)) (first tags)
                  :else (recur (rest ms) (rest tags))))]
    (check-it ns '(first second third))))

;;;
;;;    4.12
;;;
(s/fdef cycle
  :args (s/cat :n integer? :limit integer?)
  :ret  integer?)

(defn cycle [n limit]
  (inc (mod n limit)))


(s/fdef how-alike
  :args (s/cat :a integer? :b integer?)
  :ret  symbol?)

(defn how-alike [a b]
  (cond (== a b) 'the-same
        (and (odd? a) (odd? b)) 'both-odd
        (and (even? a) (even? b)) 'both-even
        (and (pos? a) (pos? b)) 'both-positive
        (and (neg? a) (neg? b)) 'both-negative
        :else 'not-alike))

(s/fdef same-sign?
  :args (s/cat :x number? :y number?)
  :ret  boolean?)

(defn same-sign? [x y]
  (or (and (zero? x) (zero? y))
      (and (neg? x) (neg? y))
      (and (pos? x) (pos? y))))

(defn same-sign? [x y]
  (cond (zero? x) (zero? y)
        (neg? x) (neg? y)
        (pos? x) (pos? y)
        :else false))

(defn same-sign? [x y]
  (or (== x y 0)
      (> (* x y) 0)))

(defn same-sign? [x y]
  (== (Math/signum (double x)) (Math/signum (double y)))) ; !!

;;;
;;;    4.15
;;;
(s/fdef geq
  :args (s/cat :x number? :y number?)
  :ret  boolean?)

(defn geq [x y]
  (or (> x y) (== x y)))

;;;
;;;    4.16
;;;
(s/fdef fancy
  :args (s/cat :x integer?)
  :ret  integer?)

(defn fancy [x]
  (cond (and (odd? x) (pos? x)) (* x x)
        (and (odd? x) (neg? x)) (* 2 x)
        :else (/ x 2)))

(defn fancy [x]
  (letfn [(fancy-factor [x]
            (if (odd? x)
              (if (pos? x) x 2)
              1/2))]
    (* x (fancy-factor x))))

;;;
;;;    4.17
;;;
(s/def ::sex '#{boy girl man woman})
(s/def ::age '#{child adult})

(s/fdef categorize
  :args (s/cat :sex ::sex :age ::age)
  :ret  boolean?)

(defn categorize [sex age]
  (case sex
    (boy girl) (= age 'child)
    (man woman) (= age 'adult)
    false))

;;;
;;;    4.18
;;;
(s/def ::gesture '#{rock paper scissors})
(s/def ::winner '#{first-player second-player tie})

(s/fdef play
  :args (s/cat :first ::gesture :second ::gesture)
  :ret  ::winner)

(defn play [first second]
  (cond (= first second) 'tie
        (or (and (= first 'rock) (= second 'paper))
            (and (= first 'paper) (= second 'scissors))
            (and (= first 'scissors) (= second 'rock)))
        'second-player
        :else 'first-player))

(def jan-ken-pon '{rock [scissors paper]
                   paper [rock scissors]
                   scissors [paper rock]})

(defn play [first second]
  (let [[beats loses] (jan-ken-pon first)]
    (cond (= first second) 'tie
          (= second beats) 'first-player
          (= second loses) 'second-player)))
