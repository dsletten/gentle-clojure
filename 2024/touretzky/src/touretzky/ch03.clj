;;;;
;;;;
;;;;   In Clojure, because the language is so bendable, you actually bend language towards the problem, not the problem towards the language.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               ch03.clj
;;;;
;;;;   Started:            Mon Sep 16 18:56:51 2024
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

(ns touretzky.ch03
  (:require [clojure.spec.alpha :as s]))

(s/fdef average
  :args (s/cat :x number? :y number?)
  :ret  number?)

(defn average [x y]
  (/ (+ x y) 2))

;;;
;;;    3.5
;;;
(s/fdef cube
  :args (s/cat :x number?)
  :ret  number?)

(defn cube [x]
  (* x x x))

;;;
;;;    3.6
;;;
(s/fdef pythag
  :args (s/cat :x number? :y number?)
  :ret  double?)

(defn pythag [x y]
  (Math/sqrt (+ (* x x) (* y y))))

;;;
;;;    3.7
;;;
(s/fdef miles-per-gallon
  :args (s/cat :initial-odometer-reading number?
               :final-odometer-reading   number?
               :gallons-consumed         number?)
  :ret  number?)

(defn miles-per-gallon [initial-odometer-reading
                        final-odometer-reading
                        gallons-consumed]
  (/ (- final-odometer-reading initial-odometer-reading) gallons-consumed))

;;;
;;;    3.11
;;;
(s/fdef longer?
  :args (s/cat :seq1 sequential?
               :seq2 sequential?)
  :ret  boolean?)

(defn longer? [seq1 seq2]
  (cond (empty? seq1) false
        (empty? seq2) true
        :else (recur (rest seq1) (rest seq2))))

;;;
;;;    No problem in Clojure
;;;    
(defn longer? [seq1 seq2]
  (> (count seq1) (count seq2)))

;;;
;;;    3.12
;;;
(s/fdef add-length
  :args (s/cat :l sequential?)
  :ret  sequential?
  :fn   (fn [{:keys [args ret]}]
          (== (inc (count (:l args))) (count ret))))

(defn add-length [l]
  (cons (count l) l))

