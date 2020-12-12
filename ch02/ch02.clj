;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch02.clj
;;;;
;;;;   Started:            Mon May  6 03:17:39 2013
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

(ns ch02
  (:use clojure.math.numeric-tower
        clojure.test
        [clojure.pprint :only (cl-format)]))

(defn make-list [a b]
  (cons a (cons b '())))


