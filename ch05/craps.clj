;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               craps.clj
;;;;
;;;;   Started:            Tue Aug 20 13:43:32 2013
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
        [clojure.pprint :only (cl-format)]))

(defn throw-die
  "Return a number from 1 to 6, inclusive, representing a throw of a 6-sided die."
  []
  (inc (rand-int 6)))

(deftest test-throw-die
  (is (every? (fn [throw] (<= 1 throw 6)) (repeatedly 10000 throw-die))))

(defn throw-dice
  "Return a sequence representing the result of throwing two 6-sided dice."
  []
  [(throw-die) (throw-die)])

(defn snake-eyes?
  "Is the throw a pair of ones?"
  [throw]
  (= throw [1 1]))

(deftest test-snake-eyes?
  (is (snake-eyes? [1 1]))
  (is (not (snake-eyes? [2 3])))
  (is (not (snake-eyes? [1]))))

(defn box-cars?
  "Is the throw a pair of sixes?"
  [throw]
  (= throw [6 6]))

(deftest test-box-cars?
  (is (box-cars? [6 6]))
  (is (not (box-cars? [2 3])))
  (is (not (box-cars? [6]))))

(defn throw-value [throw]
  (+ (first throw) (second throw)))

(defn instant-win?
  "Is the throw an instant win?"
  [throw]
;; Assert?
  (case (throw-value throw)
        (7 11) true
        false))

(deftest test-instant-win?
  (is (instant-win? [3 4]))
  (is (instant-win? [5 6]))
  (is (instant-win? [1 6]))
  (is (not (instant-win? [2 2])))
  (is (not (instant-win? [5 5]))))

(defn instant-loss?
  "Is the throw an instant loss?"
  [throw]
;; Assert?
  (case (throw-value throw)
        (2 3 12) true
        false))

(deftest test-instant-loss?
  (is (instant-loss? [1 1]))
  (is (instant-loss? [6 6]))
  (is (instant-loss? [1 2]))
  (is (not (instant-loss? [2 2])))
  (is (not (instant-loss? [5 5]))))

(defn say-throw
  "Identify a given throw."
  [throw]
;; Assert?
  (let [value (throw-value throw)]
    (case value
          2 :snake-eyes
          12 :boxcars
          value)))

(deftest test-say-throw
  (is (= (say-throw [1 1]) :snake-eyes))
  (is (= (say-throw [6 6]) :boxcars))
  (is (= (say-throw [1 2]) 3))
  (is (= (say-throw [3 4]) 7)))

(defn print-throw [throw]
  (cl-format true "Throw ~D and ~D -- " (first throw) (second throw)))

(defn craps []
  (let [throw (throw-dice)]
    (print-throw throw)
    (cond (instant-win? throw) (cl-format true "~D -- You win!~%" (say-throw throw))
          (instant-loss? throw) (cl-format true "~D -- You lose!~%" (say-throw throw))
          :else (cl-format true "Your point is ~D.~%" (say-throw throw)))) )

(defn try-for-point [point]
  (let [throw (throw-dice)
        value (throw-value throw)]
    (print-throw throw)
    (cond (== value point) (cl-format true "~D -- You win!~%" (say-throw throw))
          (== value 7) (cl-format true "~D -- You lose!~%" (say-throw throw))
          :else (cl-format true "~D -- Throw again.~%" (say-throw throw)))) )
    