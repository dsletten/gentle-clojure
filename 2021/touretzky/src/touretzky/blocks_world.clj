;;;;
;;;;
;;;;   In Clojure, because the language is so bendable, you actually bend language towards the problem, not the problem towards the language.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               blocks_world.clj
;;;;
;;;;   Started:            Fri May 28 15:32:53 2021
;;;;   Modifications:
;;;;
;;;;   Purpose: Ex. 7.29 pg. 221
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

(ns blocks-world
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

(def database '((b1 shape brick)
                (b1 color green)
                (b1 size small)
                (b1 supported-by b2)
                (b1 supported-by b3)
                (b1 material wood)
                (b2 shape brick)
                (b2 color red)
                (b2 size small)
                (b2 supports b1)
                (b2 left-of b3)
                (b2 material plastic)
                (b3 shape brick)
                (b3 color red)
                (b3 size small)
                (b3 supports b1)
                (b3 right-of b2)
                (b4 shape pyramid)
                (b4 color blue)
                (b4 size large)
                (b4 supported-by b5)
                (b5 shape cube)
                (b5 color green)
                (b5 size large)
                (b5 supports b4)
                (b6 shape brick)
                (b6 color purple)
                (b6 size large)))

(def wildcard '?)

(defn wildcard? [obj]
  (= obj wildcard))

(defn match-element [target pattern]
  (or (= target pattern)
      (wildcard? pattern)))

(deftest test-match-element
  (is (match-element 'red 'red))
  (is (match-element 'red '?))
  (is (not (match-element 'red 'blue))))

(defn match-tuple [t1 t2 n]
  (cond (empty? t1) (and (empty? t2) (zero? n))
        (or (empty? t2) (zero? n)) false
        (match-element (first t1) (first t2)) (recur (rest t1) (rest t2) (dec n))
        :else false))

(defn match-triple [assertion pattern]
  (match-tuple assertion pattern 3))

(deftest test-match-triple
  (is (match-triple '(b2 color red) '(b2 color red)))
  (is (match-triple '(b2 color red) '(? color red)))
  (is (match-triple '(b2 color red) '(b2 ? red)))
  (is (match-triple '(b2 color red) '(b2 color ?)))
  (is (not (match-triple '(b2 color) '(b2 color))))
  (is (not (match-triple '(b2 color red pung) '(b2 color red foo))))
  (is (not (match-triple '(b2 color red) '(b2 color red foo))))
  (is (not (match-triple '(b2 color red pung) '(b2 color red))))
  (is (not (match-triple '(b2 color red) '(b2 color))))
  (is (not (match-triple '(b2 color) '(b2 color red))))
  (is (not (match-triple '(b2 color red) '(b1 color red))))
  (is (not (match-triple '(b2 color red) '(b2 color green)))) )

(defn fetch [pattern]
  (filter #(match-triple % pattern) database))

(deftest test-fetch
  (is (= (fetch '(b2 color ?)) '((b2 color red))))
  (is (= (fetch '(? supports b1)) '((b2 supports b1) (b3 supports b1))))
  (is (= (fetch '(? supports ?)) '((b2 supports b1) (b3 supports b1) (b5 supports b4)))) )

(defn block-color-pattern [block]
  `(~block ~'color ~'?)) ; !!!!

(defn supporters [block]
  (map first (fetch (list '? 'supports block))))

(defn supported-by-cube? [block]
  (not (empty? (map #(fetch (list % 'shape 'cube)) (supporters block)))) )

(defn desc1 [block]
  (fetch `(~block ~'? ~'?)))

(defn desc2 [block]
  (map rest (desc1 block)))

(defn description [block]
  (apply concat (desc2 block)))

  

