;;;;
;;;;
;;;;   Programming is not about typing, it's about thinking.
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               ch08.clj
;;;;
;;;;   Started:            Wed Jun  2 20:30:56 2021
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

(ns ch08
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    8.2
;;;
(defn anyodd? [seq]
  (cond (empty? seq) false
        (odd? (first seq)) true
        :else (recur (rest seq))))

(defn anyodd? [seq]
  (if (empty? seq)
    false
    (if (odd? (first seq))
      true
      (recur (rest seq)))) )

(deftest test-anyodd?
  (is (not (anyodd? '())))
  (is (anyodd? '(7)))
  (is (not (anyodd? '(6))))
  (is (anyodd? '(6 7)))
  (is (anyodd? '(2 4 6 7 8 9))))

;;;
;;;    8.4
;;;
(defn laugh [n]
  (cond (zero? n) '()
        :else (cons '☺ (laugh (dec n)))) )
;αβγ

(deftest test-laugh
  (is (= (laugh 0) '()))
  (is (= (laugh 1) '(☺)))
  (is (= (laugh 5) '(☺ ☺ ☺ ☺ ☺))))

;;;
;;;    8.5
;;;
(defn add-up [ns]
  (cond (empty? ns) 0
        :else (+ (first ns) (add-up (rest ns)))) )

(deftest test-add-up
  (is (zero? (add-up '())))
  (is (== (add-up '(2 3 7)) 12))
  (is (zero? (add-up '(0 0 0 0))))
  (is (zero? (add-up '(1 -1 2 -2 3 -3 4 -4)))) )

;;;
;;;    8.6
;;;
(defn allodd? [ns]
  (cond (empty? ns) true
        (odd? (first ns)) (recur (rest ns))
        :else false))

(defn allodd? [ns]
  (cond (empty? ns) true
        (even? (first ns)) false
        :else (recur (rest ns))))

(deftest test-allodd?
  (is (allodd? '()))
  (is (allodd? '(1)))
  (is (allodd? '(1 3 5 7 9)))
  (is (not (allodd? '(2))))
  (is (not (allodd? '(1 2 3)))) )

;;;
;;;    8.7
;;;
(defn member [obj l]
  (cond (empty? l) false
        (= (first l) obj) l
        :else (recur obj (rest l))))

(deftest test-member
  (is (= (member 'd '(a b c d)) '(d)))
  (is (not (member 'a '())))
  (is (not (member 'e '(a b c d))))
  (is (= (member '(:c 3) '((:a 1) (:b 2) (:c 3) (:d 4))) '((:c 3) (:d 4)))) )

;;;
;;;    8.8 Common Lisp-style `assoc`. Look up key in association list.
;;;
(defn assoc [key table]
  (cond (empty? table) '()
        :else (let [[pair & more] table]
                (cond (= key (first pair)) pair
                      :else (recur key more)))) )

(deftest test-assoc
  (is (= (assoc 'c '((a 1) (b 2) (c 3))) '(c 3)))
  (is (= (assoc 'd '((a 1) (b 2) (c 3))) '()))
  (is (= (assoc 'c '(("a" 1) ("b" 2) ("c" 3))) '()))
  (is (= (assoc "c" '(("a" 1) ("b" 2) ("c" 3))) '("c" 3)))
  (is (= (assoc 5 '((0 a) (1 b) (2 c) (3 d) (4 e) (5 f))) '(5 f))))

;;;
;;;    8.9 (See 8.28 below)
;;;
(defn nth [n l]
  (if (zero? n)
    (first l)
    (recur (dec n) (rest l))))

(deftest test-nth
  (is (= (nth 0 '(a b c)) 'a))
  (is (= (nth 1 '(a b c)) 'b))
  (is (= (nth 2 '(a b c)) 'c)))
   ;; (is (= (nth 3 '(a b c)) (cl:nth 3 '(a b c)))
   ;; (is (= (nth 0 '()) (cl:nth 0 '()))) )

;;;
;;;    8.10
;;;
(defn + [x y]
  (if (neg? y)
    (throw (IllegalArgumentException. "What are you trying to pull?"))
    (loop [m x
           n y]
      (cond (zero? n) m
            :else (recur (inc m) (dec n)))) ))

(deftest test-+
  (is (every? (fn [[x y]]
                (== (+ x y) (clojure.core/+ x y)))
              (for [i (range -20 21)
                    j (range 0 21)]
                [i j]))) )

;;;
;;;    Collatz's Conjecture
;;;
(defn collatz [n]
  (cond (== n 1) true
        (even? n) (recur (/ n 2))
        :else (collatz (+ (* 3 n) 1))))

(deftest test-collatz ; !!
  (is (collatz 1))
  (is (collatz 2))
  (is (collatz 3))
  (is (collatz 4))
  (is (collatz 5))
  (is (collatz 6))
  (is (collatz 7)))

;;;
;;;    8.11
;;;
(defn fibonacci [n]
  (case n
    0 0
    1 1
    (+ (fibonacci (dec n)) (fibonacci (- n 2)))) )

(defn fibonacci [n]
  (loop [current 0
         next 1
         i 0]
    (cond (== i n) current
          :else (recur next (+ current next) (inc i)))) )

(deftest test-fibonacci
  (is (every? true? (map == (map fibonacci (range 20))
                         '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229)))) )

;;;
;;;    8.17
;;;
(defn find-first-odd [l]
  (cond (empty? l) false
        (odd? (first l)) (first l)
        :else (recur (rest l))))

(deftest test-find-first-odd
  (is (== (find-first-odd '(2 4 6 8 7)) 7))
  (is (not (find-first-odd '(2 4 6 8)))) )

;;;
;;;    Chapter 6 Little Lisper
;;;    
(defn find-first-atom [obj]
  (if (seq? obj)
    (find-first-atom (first obj))
    obj))

(defn atom? [o]
  (or (symbol? o) (number? o) (keyword? o) (boolean? o) (and (list? o) (empty? o))))

(defn leftmost
  "Return the leftmost atom in `l`."
  [l]
  (cond (empty? l) '()
        (atom? (first l)) (first l)
        :else (leftmost (first l))))

(deftest test-leftmost
  (is (= (leftmost '((hot) (tuna (and)) cheese)) 'hot))
  (is (= (leftmost '(((hamburger) french) (fries (and a) coke))) 'hamburger))
  (is (= (leftmost '((((4) four)) 17 (seventeen))) 4))
  (is (= (leftmost '(((() four)) 17 (seventeen))) '())))

(deftest test-find-first-atom
  (is (= (find-first-atom '((hot) (tuna (and)) cheese)) 'hot))
  (is (= (find-first-atom '(((hamburger) french) (fries (and a) coke))) 'hamburger))
  (is (= (find-first-atom '((((4) four)) 17 (seventeen))) 4))
  (is (= (find-first-atom '(((() four)) 17 (seventeen))) nil))) ; ??

;;;
;;;    8.18
;;;
(defn last-element
  "Find the last element of a sequence."
  [s]
  (cond (empty? s) nil
        (empty? (rest s)) (first s)
        :else (recur (rest s))))

(deftest test-last-element
  (is (nil? (last-element '())))
  (is (= (last-element '(a b c)) 'c))
  (is (== (last-element [2 4 6 8]) 8)))

;;
;;    Clojure's `last` function:
;;    
;; (def 
;;  ^{:arglists '([coll])
;;    :doc "Return the last item in coll, in linear time"
;;    :added "1.0"
;;    :static true}
;;  last (fn ^:static last [s]
;;         (if (next s)
;;           (recur (next s))
;;           (first s))))

;;;
;;;    8.21
;;;
(defn add-nums [n]
  (if (zero? n)
    0
    (clojure.core/+ n (add-nums (dec n)))) )

(defn sum [n]
  (/ (* n (inc n)) 2))

(deftest test-add-nums
  (is (== (add-nums 0) (sum 0)))
  (is (== (add-nums 1) (sum 1)))
  (is (== (add-nums 2) (sum 2)))
  (is (== (add-nums 10) (sum 10)))
  (is (== (add-nums 100) (sum 100)))
  (is (== (add-nums 1000) (sum 1000))))
  
;;;
;;;    8.22
;;;
(defn all-equal
  "Are all elements of the collection equal?"
  ([] true)
  ([x] true)
  ([x y] (= x y))
  ([x y & more] (and (all-equal x y)
                     (apply all-equal y more))))

(defn all-equal [coll]
  (println coll)
  (let [test-equal (fn test-equal ([] true)
                     ([x] true)
                     ([x y] (= x y))
                     ([x y & more] (and (test-equal x y)
                                        (all-equal (conj more y)))) )]
;                                        (all-equal (cons y more)))) )]
    (apply test-equal coll)))

(defn all-equal [coll]
  (cond (empty? coll) true
        (empty? (rest coll)) true
        :else (and (= (first coll) (second coll))
                   (all-equal (rest coll)))) )

(defn all-equal [coll]
  (cond (< (count coll) 2) true
        :else (and (= (first coll) (second coll))
                   (all-equal (rest coll)))) )

(defn all-equal [coll]
  (cond (< (count coll) 2) true
        :else (reduce #(if (= %1 %2) %2 (reduced false)) coll)))

(defn all-equal [coll]
  (or (empty? coll) (every? #(= %1 (first coll)) (rest coll))))
        
(deftest test-all-equal
  (is (all-equal '()))
  (is (all-equal '(i)))
  (is (all-equal '(i i)))
  (is (all-equal '(i i i i)))
  (is (not (all-equal '(i i e i)))))
  
;;;
;;;    8.24
;;;    
(defn count-down [n]
  (cond (zero? n) '()
        :else (cons n (count-down (dec n)))) )

(defn count-down [n]
  (loop [i 1
         result '()]
    (if (> i n)
      result
      (recur (inc i) (cons i result)))) )

(deftest test-count-down
  (is (empty? (count-down 0)))
  (is (= (count-down 10) (reverse (range 1 (inc 10)))) ))

;;;
;;;    8.25
;;;
(defn factorial [n]
  (apply * (count-down n)))

(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (dec n)))) )

(deftest test-factorial
  (is (== (factorial 0) (fact 0)))
  (is (== (factorial 1) (fact 1)))
  (is (== (factorial 5) (fact 5)))
  (is (== (factorial 20) (fact 20))))

;;;
;;;    8.26
;;;
(defn count-down-0a [n]
  (if (zero? n)
    (list n)
    (cons n (count-down-0a (dec n)))) )

(defn count-down-0b [n]
  (if (neg? n)
    '()
    (cons n (count-down-0b (dec n)))) )

(deftest test-count-down-0
  (is (all-equal (list (count-down-0a 10) (count-down-0b 10) (reverse (range 0 (inc 10)))) )))

;;;
;;;    8.27
;;;    
(defn square-list [l]
  (if (empty? l)
    '()
    (cons (#(* % %) (first l)) (square-list (rest l)))) )

(deftest test-square-list ()
  (is (= (square-list '()) (map #(* % %) '())))
  (is (= (square-list '(2)) (map #(* % %) '(2))))
  (is (= (square-list '(3 4 5 6)) (map #(* % %) '(3 4 5 6)))) )

;;;
;;;    8.28 (See 8.9 above)
;;;
(defn nth [n l]
  (cond (empty? l) nil ; Same as clojure.core/get
        (zero? n) (first l)
        :else (recur (dec n) (rest l))))

(deftest test-nth
  (is (= (nth 0 '(a b c)) 'a))
  (is (= (nth 1 '(a b c)) 'b))
  (is (= (nth 2 '(a b c)) 'c))
  (is (= (nth 3 '(a b c)) nil))
  (is (= (nth 0 '()) nil)))

