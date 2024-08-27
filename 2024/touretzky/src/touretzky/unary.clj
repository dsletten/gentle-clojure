;;;;
;;;;
;;;;   In Clojure, because the language is so bendable, you actually bend language towards the problem, not the problem towards the language.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               unary.clj
;;;;
;;;;   Started:            Wed May 15 02:18:48 2024
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

(ns touretzky.unary
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g])
  (:refer-clojure :exclude [+ - * / > >= < <= inc dec == zero? even? odd? mod min max]))

(def ^:const tally 'X)
(s/def ::tally #{tally})
(s/def ::number (s/coll-of ::tally :into ()))
;;    Need to define generator
;(s/def ::number (s/coll-of ::tally :kind #(and (sequential? %) (not (vector? %)))))

(def zero '())

(s/fdef zero?
  :args (s/cat :n ::number)
  :ret  boolean?)

;; (empty? n)
(defn zero? [n]
  (= n zero))

(s/fdef inc
  :args (s/cat :n ::number)
  :ret  ::number)

(defn inc [n]
  (cons tally n))

(def one (inc zero))

(s/fdef one?
  :args (s/cat :n ::number)
  :ret  boolean?)

(defn one? [n]
  (= n one))

(s/fdef dec
  :args (s/cat :n ::number)
  :ret  ::number)

(defn dec [n]
  (if (zero? n)
    (throw (IllegalArgumentException. "Cannot decrement zero."))
    (rest n)))

;;;
;;;    Cribbed from clojure.core!
;;;    
;; (defn ==
;;   ([m] true)
;;   ([m n] (cond (zero? m) (zero? n)
;;                (zero? n) false
;;                :else (recur (dec m) (dec n))))
;;   ([m n & more] (if (== m n)
;;                   (if (next more)
;;                     (recur n (first more) (next more))
;;                     (== n (first more)))
;;                   false)))

(defn- operator [f m ms]
  (or (empty? ms)
      (let [[n & ns] ms]
        (and (f m n)
             (recur f n ns)))) )

(s/fdef ==
  :args (s/cat :m ::number :ms (s/* ::number))
  :ret  boolean?)

(defn == [m & ms]
  (letfn [(equals [m n]
            (cond (zero? m) (zero? n)
                  (zero? n) false
                  :else (recur (dec m) (dec n))))]
    (operator equals m ms)))

(s/fdef >
  :args (s/alt :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  boolean?)

(defn >
  ([m] true)
  ([m n] (cond (zero? m) false
               (zero? n) true
               :else (recur (dec m) (dec n))))
  ([m n & more]
   (if (> m n)
     (if (next more)
       (recur n (first more) (next more)) ; Some magic with `recur` and & more!
       (> n (first more)))
     false)))

;; (defn < 
;;   ([m] true)
;;   ([m n] (> n m))
;;   ([m n & more]
;;    (if (< m n)
;;      (if (next more)
;;        (recur m (first more) (next more))
;;        (< n (first more)))
;;      false)))

(s/fdef <
  :args (s/alt :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  boolean?)

(defn <
  ([m] true)
  ([m n] (cond (zero? n) false
               (zero? m) true
               :else (recur (dec m) (dec n))))
  ([m n & more]
   (if (< m n)
     (if (next more)
       (recur n (first more) (next more))
       (< n (first more)))
     false)))

(s/fdef >=
  :args (s/alt :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  boolean?)

;; (defn >= [& more]
;;   (or (apply > more)
;;       (apply == more)))

;;;
;;;    This is wrong!
;;;    (>= 4 3 3 2) => (and (>= 4 3) (>= 3 3) (>= 3 2))
;;;    (not (< 4 3 3 2)) => (not (and (< 4 3) (< 3 3) (< 3 2)))
;;;                      => (or (not (< 4 3)) (not (< 3 3)) (not (< 3 2)))
;;;                      => (or (>= 4 3) (>= 3 3) (>= 3 2))
;;;                      
;; (defn >=
;;   ([m] true) ; Essential!
;;   ([m & more] (not (apply < (cons m more)))) )
(defn >=
  ([m] true)
  ([m n] (cond (zero? m) (zero? n) ; These cases are a hybrid of == and >
               (zero? n) true
               :else (recur (dec m) (dec n))))
  ([m n & more]
   (if (>= m n)
     (if (next more)
       (recur n (first more) (next more))
       (>= n (first more)))
     false)))

(s/fdef <=
  :args (s/alt :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  boolean?)

;;;
;;;    Also wrong!
;;;    (<= 2 3 3) but (< 2 3 3) => false and (== 2 3 3) => false
;;;    
;; (defn <= [& more]
;;   (or (apply < more)
;;       (apply == more)))

(defn <=
  ([m] true)
  ([m n] (cond (zero? n) (zero? m) ; These cases are a hybrid of == and <
               (zero? m) true
               :else (recur (dec m) (dec n))))
  ([m n & more]
   (if (<= m n)
     (if (next more)
       (recur n (first more) (next more))
       (<= n (first more)))
     false)))

(s/fdef +
  :args (s/alt :nullary  (s/cat)
               :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  ::number)

(defn +
  ([] zero)
  ([m] m)
  ([m n] (if (zero? n)
           m
           (recur (inc m) (dec n))))
  ([m n & more] (reduce + (+ m n) more)))

(s/fdef -
  :args (s/alt :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  ::number)

;; (defn -
;;   ([m n] (cond (zero? n) m
;;                (zero? m)
;;                (throw (IllegalArgumentException. "Cannot subtract from zero."))
;;                :else (try
;; ;                       (recur (dec m) (dec n))
;;                        (- (dec m) (dec n))
;;                        (catch IllegalArgumentException _
;;                          (throw (IllegalArgumentException. (str "Cannot subtract "
;;                                                                 n " from " m)))) )))
;;   ([m n & more] (reduce - (- m n) more)))

;;;
;;;    Single arity is not meaningful here.
;;;    
(defn -
  ([m n]
    (try
      (loop [minuend m
             subtrahend n]
        (if (zero? subtrahend)
          minuend
          (recur (dec minuend) (dec subtrahend))))
      (catch IllegalArgumentException _
        (throw (IllegalArgumentException. (str "Cannot subtract " n " from " m)))) ))
  ([m n & more] (reduce - (- m n) more)))

(s/fdef *
  :args (s/alt :nullary  (s/cat)
               :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  ::number)

;; (defn *
;;   ([] one)
;;   ([m] m)
;;   ([m n] (if (or (zero? m) (zero? n))
;;            zero
;;            (+ m (* m (dec n)))))
;;   ([m n & more] (reduce * (* m n) more)))

(defn *
  ([] one)
  ([m] m)
  ([m n]
   (if (zero? m)
     zero
     (loop [multiplier n
            result zero]
       (if (zero? multiplier)
         result
         (recur (dec multiplier) (+ m result)))) ))
  ([m n & more] (reduce * (* m n) more)))

(s/fdef /
  :args (s/alt :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  ::number)

;; (defn /
;;   ([m n] (cond (zero? n)
;;                (throw (IllegalArgumentException. "Cannot divide by zero."))
;;                (< m n) zero
;;                :else
;;                (inc (/ (- m n) n))))
;;   ([m n & more] (reduce / (/ m n) more)))

(defn /
  ([m n]
   (cond (zero? n)
         (throw (IllegalArgumentException. "Cannot divide by zero."))
         :else
         (loop [dividend m
                result zero]
           (if (< dividend n)
             result
             (recur (- dividend n) (inc result)))) ))
  ([m n & more] (reduce / (/ m n) more)))

(s/fdef mod
  :args (s/cat :m ::number :n ::number)
  :ret  ::number)

(defn mod
  ([m n] (cond (zero? n) (throw (IllegalArgumentException. "Cannot divide by zero."))
               (< m n) m
               :else (recur (- m n) n))))

(declare odd?)

(s/fdef even?
  :args (s/cat :n ::number)
  :ret  boolean?)

(defn even? [n]
  (cond (zero? n) true
        (one? n) false
        :else (odd? (dec n))))

(s/fdef odd?
  :args (s/cat :n ::number)
  :ret  boolean?)

(defn odd? [n]
  (cond (zero? n) false
        (one? n) true
        :else (even? (dec n))))

(s/fdef min
  :args (s/alt :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  ::number)

(defn min
  ([m] m)
  ([m n] (if (< m n) m n))
  ([m n & more] (reduce min (min m n) more)))

(s/fdef max
  :args (s/alt :unary    (s/cat :m ::number)
               :binary   (s/cat :m ::number :n ::number)
               :variadic (s/cat :m    ::number
                                :n    ::number
                                :more (s/* ::number)))
  :ret  ::number)

(defn max
  ([m] m)
  ([m n] (if (> m n) m n))
  ([m n & more] (reduce max (max m n) more)))

