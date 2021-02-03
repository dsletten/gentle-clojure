;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               craps.clj
;;;;
;;;;   Started:            Sun Jan 24 16:47:56 2021
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

(ns craps
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

(defn throw-die []
  (inc (rand-int 6)))

(defn throw-dice []
  (list (throw-die) (throw-die)))

(defn snake-eyes? [throw]
  (= throw '(1 1)))

(defn box-cars? [throw]
  (= throw '(6 6)))

(defn value [throw]
  (reduce + throw))

(defn instant-win? [throw]
  (case (value throw)
    (7 11) true
    false))

(defn instant-loss? [throw]
  (case (value throw)
    (2 3 12) true
    false))

(defn say-throw [throw]
  (cond (snake-eyes? throw) 'snake-eyes
        (box-cars? throw) 'box-cars
        :else (value throw)))

(defn outcome 
  ([throw] (cond (instant-win? throw) '(you win)
                 (instant-loss? throw) '(you lose)
                 :else `(~'your ~'point ~'is ~(value throw))))
  ([throw point] (cond (== (value throw) 7) '(you lose)
                       (== (value throw) point) '(you win)
                       :else '(throw again))))

(defn make-report [[first second] label outcome]
  `(~'throw ~first ~'and ~second ~'-- ~label ~'-- ~@outcome))

(defn craps []
  (let [throw (throw-dice)]
    (make-report throw (say-throw throw) (outcome throw))))

(defn try-for-point [point]
  (let [throw (throw-dice)]
    (make-report throw (say-throw throw) (outcome throw point))))
