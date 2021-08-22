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

;;;
;;;    8.31
;;;
(defn compare-lengths [l1 l2]
  (cond (empty? l1) (if (empty? l2) 'same-length 'second-is-longer)
        (empty? l2) 'first-is-longer
        :else (recur (rest l1) (rest l2))))

(defn compare-lengths [l1 l2]
  (cond (and (empty? l1) (empty? l2)) 'same-length
        (empty? l1) 'second-is-longer
        (empty? l2) 'first-is-longer
        :else (recur (rest l1) (rest l2))))

(deftest test-compare-lengths
  (is (= (compare-lengths '() '()) 'same-length))
  (is (= (compare-lengths '(a b c) '(1 2 3)) 'same-length))
  (is (let [l (take 200 (repeat 'foo))] (= (compare-lengths l l) 'same-length)))
  (is (= (compare-lengths '() '(1)) 'second-is-longer))
  (is (= (compare-lengths '(a b c) '(1 2 3 4)) 'second-is-longer))
  (is (let [l (take 200 (repeat 'foo))] (= (compare-lengths (rest l) l) 'second-is-longer)))
  (is (= (compare-lengths '(a) '()) 'first-is-longer))
  (is (= (compare-lengths '(a b c d) '(1 2 3)) 'first-is-longer))
  (is (let [l (take 200 (repeat 'foo))] (= (compare-lengths l (rest l)) 'first-is-longer))))

;;;
;;;    8.32
;;;
(defn sum-numeric-elements [l]
  (cond (empty? l) 0
        (number? (first l)) (+ (first l) (sum-numeric-elements (rest l)))
        :else (sum-numeric-elements (rest l))))

(defn sum-numeric-elements [l]
  (loop [sum 0
         list l]
    (cond (empty? list) sum
          (number? (first list)) (recur (+ (first list) sum) (rest list))
          :else (recur sum (rest list)))) )

(deftest test-sum-numeric-elements
  (is (== (sum-numeric-elements '(3 bears 3 bowls and 1 girl)) 7))
  (is (== (sum-numeric-elements '()) 0))
  (is (== (sum-numeric-elements '(a 1 b 2 c 3 4 5 6 x)) 21)))

;;;
;;;    8.33
;;;
(defn remove [obj l & {:keys [test key] :or {test = key nil}}]
  (cond (empty? l) '()
        (nil? key) (if (test obj (first l))
                     (remove obj (rest l) :test test)
                     (cons (first l) (remove obj (rest l) :test test)))
        :else (if (test obj (key (first l)))
                (remove obj (rest l) :test test :key key)
                (cons (first l) (remove obj (rest l) :test test :key key)))) )

(deftest test-remove
  (is (= (remove 'a '()) '()))
  (is (= (remove 'a '(b c d)) '(b c d)))
  (is (= (remove 'a '(a b c a d a b)) '(b c d b)))
  (is (= (remove 1.0 '(1 2 1.0 3 1)) '(1 2 3 1)))
  (is (= (remove 1.0 '(1 2 1.0 3 1) :test ==) '(2 3)))
  (is (= (remove 1.0 '(1 2 1.0 3 1) :key float) '(2 3)))
  (is (= (remove :z '((a 1) (b 2) (:z 3)) :key first) '((a 1) (b 2)))) )

;;;
;;;    8.34
;;;
(defn intersection [a b]
  (cond (empty? a) '()
        (member (first a) b) (cons (first a) (intersection (rest a) b))
        :else (intersection (rest a) b)))

(defn set= [a b]
  (= (set a) (set b)))

(deftest test-intersection
  (is (set= (intersection '(a b) '(a b)) '(a b)))
  (is (set= (intersection '(a b c d) '(d a c b)) '(a b c d)))
  (is (set= (intersection '(a b c d) '(d b)) '(b d)))
  (is (set= (intersection '(a b c) '(d e f)) '()))
  (is (set= (intersection '(b c a) '(a d e)) '(a)))
  (is (set= (intersection '() '(a b c d)) '()))
  (is (set= (intersection '(1 2 3 4) '()) '())))

;;;
;;;    8.35
;;;
(defn set-difference [a b]
  (cond (empty? a) '()
        (member (first a) b) (set-difference (rest a) b)
        :else (cons (first a) (set-difference (rest a) b))))

(deftest test-set-difference
  (is (set= (set-difference '(a b) '(a b)) '()))
  (is (set= (set-difference '(a b c d) '(d a c b)) '()))
  (is (set= (set-difference '(a b c) '(d e f)) '(a b c)))
  (is (set= (set-difference '(b c a) '(a d e)) '(b c)))
  (is (set= (set-difference '() '(a b c d)) '()))
  (is (set= (set-difference '(a b c d) '()) '(a b c d))))

;;;
;;;    8.36
;;;
(defn count-odd [l]
  (cond (empty? l) 0
        (odd? (first l)) (inc (count-odd (rest l)))
        :else (count-odd (rest l))))

(defn count-odd [l]
  (cond (empty? l) 0
        :else (+ (if (odd? (first l)) 1 0) (count-odd (rest l)))) )

(deftest test-count-odd
  (is (== (count-odd '()) 0))
  (is (== (count-odd '(2 4 6 8)) 0))
  (is (== (count-odd '(4 5 6 7 8)) 2)))

;;;
;;;    8.39
;;;
;; (defn count-atoms [tree]
;;   (cond (seq? tree) (+ (count-atoms (first tree))
;;                        (count-atoms (rest tree)))
;;         :else 1))

(defn count-atoms [tree]
  (cond (atom? tree) 1 ; ?????????
        :else (+ (count-atoms (first tree))
                 (count-atoms (rest tree)))) )

(deftest test-count-atoms
  (is (== (count-atoms '()) 1))
  (is (== (count-atoms '(a b c)) 4))
  (is (== (count-atoms '(a (b) c)) 5))
  (is (== (count-atoms '((a b) c)) 5)))
  
;;;
;;;    8.40
;;;
(defn count-cons [tree]
  (if (seq? tree)
    (if (empty? tree)
      0
      (clojure.core/+ 1 (count-cons (first tree)) (count-cons (rest tree))))
    0))

(deftest test-count-cons
  (is (== (count-cons '()) 0))
  (is (== (count-cons 'a) 0))
  (is (== (count-cons '(a)) 1))
  (is (== (count-cons '((a))) 2))
  (is (== (count-cons '(a b)) 2))
  (is (== (count-cons '(a b c)) 3)))

;;;
;;;    8.41
;;;
;; (defn sum-tree [tree]
;;   (cond (number? tree) tree
;;         (seq? tree) (+ (sum-tree (first tree))
;;                        (sum-tree (rest tree)))
;;         :else 0))

(defn sum-tree [tree]
  (if (seq? tree)
    (if (empty? tree)
      0
      (+ (sum-tree (first tree))
         (sum-tree (rest tree))))
    (if (number? tree)
      tree
      0)))

(deftest test-sum-tree
  (is (== (sum-tree 5) 5))
  (is (== (sum-tree 'a) 0))
  (is (== (sum-tree '()) 0))
  (is (== (sum-tree '((a 1) b 2 (c 3))) 6))
  (is (== (sum-tree '((3 bears) (3 bowls) (1 girl))) 7)))

;;;
;;;    8.42
;;;
(defn subst [new old tree & {:keys [test key] :or {test = key nil}}]
  (if (nil? key)
    (cond (test old tree) new
          (seq? tree) (if (empty? tree)
                        tree
                        (cons (subst new old (first tree) :test test)
                              (subst new old (rest tree) :test test)))
          :else tree)
    (cond (test old (key tree)) new
          (seq? tree) (if (empty? tree)
                        tree
                        (cons (subst new old (first tree) :key key :test test)
                              (subst new old (rest tree) :key key :test test)))
          :else tree)))
    
(deftest test-subst
  (is (= (subst 'topping 'pickle '(ice cream (with (fudge)) for dessert)) '(ice cream (with (fudge)) for dessert)))
  (is (= (subst 'topping 'fudge '(ice cream (with (fudge)) for dessert)) '(ice cream (with (topping)) for dessert)))
  (is (= (subst 'jalapeno 'and '(tacos (tamales (and (salsa))))) '(tacos (tamales (jalapeno (salsa)))) ))
  (is (= (let [pi Math/PI] (subst 'X pi `(2 3 ~pi (~pi (~pi)) 4) :test (fn [old elt] (and (number? elt) (== old elt)))) )
         '(2 3 X (X (X)) 4)))
  (is (= (subst 'X 3 '(1 (1 2) (1 2 3) (1 2 3 4)) :key (fn [y] (and (list? y) (second (rest y)))) )
         '(1 (1 2) X X))))
      
;;;
;;;    8.43
;;;
(defn flatten [tree]
  (cond (seq? tree)
        (cond (empty? tree) tree
              (seq? (first tree))
              (cond (empty? (first tree)) (flatten (rest tree))
                    :else (flatten (list* (first (first tree)) (rest (first tree)) (rest tree))))
              :else (cons (first tree) (flatten (rest tree))))
        :else tree))

(defn flatten [tree]
  (let [flatten-aux (fn [tree result]
                      (cond (empty? tree) (reverse result)
                            (seq? (first tree)) (if (empty? (first tree))
                                                  (recur (rest tree) result)
                                                  (let [[[head & tail1] & tail2] tree]
                                                    (recur (list* head tail1 tail2) result)))
                            (nil? (first tree)) (recur (rest tree) result)  ; ?!?!?!?!?
                            :else (recur (rest tree) (cons (first tree) result))))]
    (if (seq? tree)
      (flatten-aux tree '())
      tree)))

(deftest test-flatten
  (is (= (flatten '((a b (r)) a c (a d ((a (b)) r) a))) '(a b r a c a d a b r a)))
  (is (= (flatten '((((((((0) (((1) (2) (3) (4) (((((5) 6) (7) (8) 9) 10) 11) (12) (13) 14) 15) ((16) ((((((17) 18) (19) (((20) 21) ((((22) (23) 24) 25) 26) (27) 28) 29) (30) 31) (32) (33) (34) 35) 36) 37)
                          (38) 39) ((40) 41) ((((((42) ((((43) ((((44) (((45) (46) 47) ((48) (49) (50) ((51) 52) (53) (54) 55) 56) (57) 58) 59) (60) 61) 62) 63) (64) 65) ((66) 67) 68) 69) 70) 71) 72) (73) 74)
                        (75) 76) 77) (78) (79) 80) (((81) (82) 83) 84) 85) 86))
         '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
             55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86))))

;;;
;;;    8.44
;;;
(defn tree-depth [tree]
  (cond (or (not (seq? tree)) (empty? tree)) 0
        :else (inc (max (tree-depth (first tree))
                        (tree-depth (rest tree)))) ))

(deftest test-tree-depth
  (is (== (tree-depth '()) 0)) ; ?
  (is (== (tree-depth '(())) 1)) ; ?
  (is (== (tree-depth '((()))) 2)) ; ?
  (is (== (tree-depth 'a) 0))
  (is (== (tree-depth '(a)) 1))
  (is (== (tree-depth '(a b c d e f g h i j k l)) 12))
  (is (== (tree-depth '((a b c d))) 5))
  (is (== (tree-depth '((a b) ((c)) (d))) 4)))

;;;
;;;    8.45
;;;
(defn paren-depth [tree]
  (cond (or (not (seq? tree)) (empty? tree)) 0
        :else (max (inc (paren-depth (first tree)))
                   (paren-depth (rest tree)))) )

(deftest test-paren-depth
  (is (== (paren-depth '()) 0)) ; ?
  (is (== (paren-depth '(())) 1)) ; ?
  (is (== (paren-depth '((()))) 2)) ; ?
  (is (== (paren-depth 'a) 0))
  (is (== (paren-depth '(a)) 1))
  (is (== (paren-depth '(a b c)) 1))
  (is (== (paren-depth '(a b c d e f g h i j k l)) 1))
  (is (== (paren-depth '((a) (b) (c))) 2))
  (is (== (paren-depth '(a b ((c) d) e)) 3))
  (is (== (paren-depth '(((a)) ((b)) ((c)))) 3))
  (is (== (paren-depth '((a b) ((c)) (d))) 3)))

;;;
;;;    8.49
;;;    
(defn pairings [xs ys]
  (cond (empty? xs) '()
        (empty? ys) '()
        :else (cons (list (first xs) (first ys)) (pairings (rest xs) (rest ys)))) )

(deftest test-pairings
  (is (= (pairings '() '()) '()))
  (is (= (pairings '(a) '()) '()))
  (is (= (pairings '() '(1)) '()))
  (is (= (pairings '(a b c) '(1 2 3)) '((a 1) (b 2) (c 3))))
  (is (= (pairings '(a b) '(1 2 3)) '((a 1) (b 2))))
  (is (= (pairings '(a b c) '(1 2)) '((a 1) (b 2)))) )

;;;
;;;    8.50
;;;
(defn sublists [l]
  (cond (empty? l) '()
        :else (cons l (sublists (rest l)))) )

(deftest test-sublists
  (is (= (sublists '()) '()))
  (is (= (sublists '(a)) '((a))))
  (is (= (sublists '(fee fie foe)) '((fee fie foe) (fie foe) (foe)))) )

;;;
;;;    8.51
;;;
(defn reverse [l]
  (let [reverse-it (fn [l result]
                     (cond (empty? l) result
                           :else (recur (rest l) (cons (first l) result))))]
    (reverse-it l '())))

(defn reverse [l]
  (loop [l l
         result '()]
    (cond (empty? l) result
          :else (recur (rest l) (cons (first l) result)))) )

(deftest test-reverse
  (is (= (reverse '()) '()))
  (is (= (reverse '(a)) '(a)))
  (is (= (reverse '(a b c d)) '(d c b a)))
  (is (= (reverse '(a b (c (d)) (e))) '((e) (c (d)) b a))))

;;;
;;;    8.52
;;;    
(defn union [a b]
  (cond (empty? a) b
        (member (first a) b) (recur (rest a) b)
        :else (recur (rest a) (cons (first a) b))))

(deftest test-union
  (is (set= (union '(a b) '(a b)) '(a b)))
  (is (set= (union '(a b c d) '(d a c b)) '(a b c d)))
  (is (set= (union '(a b c d) '(d b)) '(a b c d)))
  (is (set= (union '(a b c) '(d e f)) '(a b c d e f)))
  (is (set= (union '(b c a) '(a d e)) '(a b c d e)))
  (is (set= (union '() '(a b c d)) '(a b c d)))
  (is (set= (union '(1 2 3 4) '()) '(1 2 3 4))))

;;;
;;;    8.53
;;;
(defn largest-even [l]
  (loop [n 0
         l l]
    (cond (empty? l) n
          (even? (first l)) (recur (max (first l) n) (rest l))
          :else (recur n (rest l)))) )

(deftest test-largest-even
  (is (zero? (largest-even '())))
  (is (zero? (largest-even '(1 3 5 7))))
  (is (== (largest-even '(2)) 2))
  (is (== (largest-even '(1 2 3 4 5)) 4))
  (is (== (largest-even '(2 6 16 6 0 2 2 12 8 0)) 16)))

;;;
;;;    8.54
;;;
;;;
;;;    Whoops! Goofed this up...
;;;    
;; (defn huge [n]
;;   (loop [m 1
;;          i n]
;; (println m i)
;;     (cond (zero? i) 1
;;           (== i 1) n
;;           (even? i) (recur (* m m) (/ i 2))
;;           :else (recur (* m n) (dec i)))) )

;; (defn huge [n]
;;   (cond (zero? n) :undefined
;;         :else (loop [m n
;;                      i (dec n)]
;;                 (println m i)
;;                 (cond (zero? i) m
;; ;                  (== i 1) m
;;                   (even? i) (recur (* m m) (/ i 2))
;;                   :else (recur (* m n) (dec i)))) ))

(defn huge [n]
  (if (zero? n)
    :undefined
    (loop [m 1
           i n]
      (if (zero? i)
        m
        (recur (* m n) (dec i)))) ))

(defn huge [n]
  (let [power (fn power [m i]
                (cond (zero? i) 1
                      (== i 2) (*' m m)
                      (even? i) (power (power m (/ i 2)) 2)
                      :else (*' m (power m (dec i)))) )]
  (if (zero? n)
    :undefined
    (power n n))))

(deftest test-huge
  (is (== (huge 1) 1))
  (is (== (huge 2) 4))
  (is (== (huge 3) 27))
  (is (== (huge 4) 256))
  (is (== (huge 11) (Math/pow 11 11)))
  (is (== (huge 140) (Math/pow 140 140))))
;  (is (== (huge 150) (Math/pow 150 150))))
;                      ^^^^^^^^^^^^^^^^ No can do!         

;;;
;;;    8.56
;;;
(defn every-other [l]
  (letfn [(tweedle-dee [l]
            (if (empty? l)
              '()
              (cons (first l) (tweedle-dum (rest l)))) )
          (tweedle-dum [l]
            (if (empty? l)
              '()
              (tweedle-dee (rest l)))) ]
    (tweedle-dee l)))

(defn every-other [l]
  (let [select (fn [l result]
                 (cond (empty? l) (reverse result)
                       (empty? (rest l)) (reverse (cons (first l) result))
                       :else (recur (rest (rest l)) (cons (first l) result))))]
    (select l '())))

(deftest test-every-other
  (is (= (every-other '()) '()))
  (is (= (every-other '(a)) '(a)))
  (is (= (every-other '(a b)) '(a)))
  (is (= (every-other '(a b c)) '(a c)))
  (is (= (every-other '(a b c d e f g)) '(a c e g)))
  (is (= (every-other '(i came i saw i conquered)) '(i i i))))

;;;
;;;    8.57
;;;    
(defn left-half [l]
  (let [half (fn half [l n]
               (if (zero? n)
                 '()
                 (cons (first l) (half (rest l) (dec n)))) )]
    (half l (int (Math/ceil (/ (count l) 2)))) ))

(defn left-half [l]
  (take (int (Math/ceil (/ (count l) 2))) l))

;;;
;;;    Chase 2 tails. When faster tail (by CDDR) reaches end, slower tail
;;;    is halfway.
;;;    
(defn left-half [l]
  (let [half (fn half [l l1]
               (cond (empty? l1) '()
                     :else (cons (first l) (half (rest l) (rest (rest l1)))) ))]
    (half l l)))

(deftest test-left-half
  (is (= (left-half '()) '()))
  (is (= (left-half '(a)) '(a)))
  (is (= (left-half '(a b)) '(a)))
  (is (= (left-half '(a b c)) '(a b)))
  (is (= (left-half '(a b c d e)) '(a b c)))
  (is (= (left-half '(1 2 3 4 5 6 7 8)) '(1 2 3 4))))

(defn right-half [l]
  (let [half (fn [l i sublists]
               (cond (empty? l) (nth (quot (dec i) 2) sublists)
                     :else (recur (rest l) (inc i) (cons l sublists))))]
    (if (empty? l)
      '()
      (half l 0 '()))) )

(defn right-half [l]
  (drop (quot (count l) 2) l))

(defn right-half [l]
  (let [half (fn [l l1]
               (cond (empty? (rest l1)) l
                     :else (recur (rest l) (rest (rest l1)))) )]
    (half l l)))

(deftest test-right-half
  (is (= (right-half '()) '()))
  (is (= (right-half '(a)) '(a)))
  (is (= (right-half '(a b)) '(b)))
  (is (= (right-half '(a b c)) '(b c)))
  (is (= (right-half '(a b c d e)) '(c d e)))
  (is (= (right-half '(1 2 3 4 5 6 7 8)) '(5 6 7 8))))

;;;
;;;    8.58
;;;
(defn merge-lists [xs ys]
  (cond (empty? xs) ys
        (empty? ys) xs
        (< (first ys) (first xs)) (cons (first ys) (merge-lists xs (rest ys)))
        :else (cons (first xs) (merge-lists (rest xs) ys))))

(defn merge-lists [xs ys]
  (loop [xs xs
         ys ys
         result []]
    (cond (empty? xs) (concat result ys)
          (empty? ys) (concat result xs)
          (< (first ys) (first xs)) (recur xs (rest ys) (conj result (first ys)))
          :else (recur (rest xs) ys (conj result (first xs)))) ))

(deftest test-merge-lists
  (is (= (merge-lists '() '()) '()))
  (is (= (merge-lists '(1) '()) '(1)))
  (is (= (merge-lists '() '(1)) '(1)))
  (is (= (merge-lists '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)))
  (is (= (merge-lists '(4 5 6) '(1 2 3)) '(1 2 3 4 5 6)))
  (is (= (merge-lists '(1 2 6 8 10 12) '(2 3 5 9 13)) '(1 2 2 3 5 6 8 9 10 12 13)))
  (is (= (merge-lists '(1 2 6 8 10 12) '(2.0 3 5 9 13)) '(1 2 2.0 3 5 6 8 9 10 12 13))))
