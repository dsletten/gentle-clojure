;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               craps.clj
;;;;
;;;;   Started:            Mon Nov 11 04:18:13 2019
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

(def snake-eyes [1 1])
(def boxcars [6 6])

(defn snake-eyes? [throw]
  (= throw snake-eyes))

(defn boxcars? [throw]
  (= throw boxcars))

(defn throw-die []
  (inc (rand-int 6)))

(defn throw-dice []
  [(throw-die) (throw-die)])

(defn throw-sum [throw]
  (apply + throw))

(defn instant-win? [throw]
  (case (throw-sum throw)
    (7 11) true
    false))

(defn instant-loss? [throw]
  (case (throw-sum throw)
    (2 3 12) true
    false))

(defn say-throw [throw]
  (cond (snake-eyes? throw) 'snake-eyes
        (boxcars? throw) 'boxcars
        :else (throw-sum throw)))

(defn outcome [throw]
  (cond (instant-win? throw) '(you win)
        (instant-loss? throw) '(you lose)
        :else `(~'your ~'point ~'is ~(say-throw throw)))) ; !!!!!

(defn announce [throw outcome]
  `(~'throw ~(first throw) ~'and ~(second throw) ~'-- ~(say-throw throw) ~'-- ~@outcome))

(defn craps []
  (let [throw (throw-dice)]
    (announce throw (outcome throw))))

(defn try-for-point [point]
  (let [throw (throw-dice)
        score (throw-sum throw)]
    (cond (== score point) (announce throw '(you win))
          (== score 7) (announce throw '(you lose))
          :else (announce throw '(throw again)))) )

;;;
;;;    2020
;;;

(defn throw-die []
  (inc (rand-int 6)))

(defn throw-dice []
  (list (throw-die) (throw-die)))

(defn snake-eyes? [throw]
  (= throw '(1 1)))

(defn box-cars? [throw]
  (= throw '(6 6)))

(defn instant-win? [throw]
  (case (+ (first throw) (second throw))
    (7 11) true
    false))

(defn instant-loss? [throw]
  (case (+ (first throw) (second throw))
    (2 3 12) true
    false))

(defn say-throw [throw]
  (cond (snake-eyes? throw) 'snake-eyes
        (box-cars? throw) 'box-cars
        :else (+ (first throw) (second throw))))

(defn say [first second result status]
  `(~'throw ~first ~'and ~second ~'-- ~result ~'-- ~@status))

(defn craps []
  (let [throw (throw-dice)
        [first second] throw
        result (say-throw throw)]
    (cond (instant-win? throw) (say first second result '(you win))
          (instant-loss? throw) (say first second result '(you lose))
          :else (say first second result `(~'your ~'point ~'is ~result)))) )

(defn try-for-point [point]
  (let [[first second] (throw-dice)
        result (+ first second)]
    (cond (== result point) (say first second result '(you win))
          (== result 7) (say first second result '(you lose))
          :else (say first second result '(throw again)))) )

