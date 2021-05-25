;;;;
;;;;
;;;;   With Clojure we found that the very very low friction to get things done enables you to do things that you'd otherwise never even consider
;;;;   -- Orestis Markou
;;;;
;;;;   Name:               ch07.clj
;;;;
;;;;   Started:            Fri Apr 16 10:49:25 2021
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

(ns ch07
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    7.7
;;;
(defn flip
  "Flip elements of `l` from `up` to `down` and vice versa."
  [l]
  (map (fn [elt] (case 'up 'down 'down 'up)) l))

(deftest test-flip
  (is (flip '()) '())
  (is (flip '(up down up up)) '(down up down down))
  (is (flip '(up up up)) '(down down down))
  (is (flip '(down down down)) '(up up up)))

;;;
;;;    (find-if f l) ≈ (first (filter f l))
;;;    

;;;
;;;    7.8
;;;    
(defn find-in-ballpark
  "Find the first element of `xs` that is within `ballpark` of `target`."
  ([target xs] (find-in-ballpark target xs 10))
  ([target xs ballpark]
   (first (filter #(<= (- target ballpark) % (+ target ballpark)) xs))))

(deftest test-find-in-ballpark
  (is (== (find-in-ballpark 1 '(2 4 6 8 10)) 2))
  (is (== (find-in-ballpark 19 '(2 4 6 8 10)) 10))
  (is (== (find-in-ballpark 5 '(2 4 6 8 10) 1) 4))
  (is (== (find-in-ballpark 5 '(2 4 6 8 10) 1.1) 4))
  (is (== (find-in-ballpark 6 '(2 4 6 8 10)) 2))
  (is (== (find-in-ballpark 6 '(2 4 6 8 10) 0.5) 6)))

;;;
;;;    7.9
;;;
(defn find-nested [l]
  (first (filter #(and (seq? %) (seq %)) l)))

(deftest test-find-nested
  (is (= (find-nested '(a b (c d) (e f))) '(c d)))
  (is (not (find-nested '(a b c))))
  (is (not (find-nested '(a b () c)))) )

;;;
;;;    7.11
;;;
(defn handful [seq]
  (filter #(< 1 % 5) seq))

(deftest test-handful
  (is (= (handful (range -1.0 7.0 0.5)) '(1.5 2.0 2.5 3.0 3.5 4.0 4.5)))
  (is (empty? (handful '(8 9 10 11 12)))) )

;;;
;;;    7.12
;;;
(defn count-the [sentence]
  (count (filter #(= % 'the) sentence)))

(deftest test-count-the
  (is (== (count-the '(a fool for your loving no more)) 0))
  (is (== (count-the '(now is the time for all good men to come to the aid of their country)) 2))
  (is (== (count-the '(the quick brown fox jumps over the lazy dog)) 2)))

;;;
;;;    7.13
;;;
(defn find-twofers [l]
  (filter #(and (seq? %) (= (count %) 2)) l))

(deftest test-find-twofers
  (is (= (find-twofers '(a b c)) '()))
  (is (= (find-twofers '((a) (b) (c))) '()))
  (is (= (find-twofers '((a 1) (b 2) (c 3))) '((a 1) (b 2) (c 3))))
  (is (= (find-twofers '((a 1) (b 2 :x) (c 3) (d 4) (e 5 fight like a brave))) '((a 1) (c 3) (d 4)))) )

;;;
;;;    7.14
;;;
(defn member [o l]
  (cond (empty? l) false
        (= o (first l)) true
        :else (recur o (rest l))))

(defn set-difference [a b]
  (remove #(member % b) a))

(deftest test-set-difference
  (is (= (set-difference '(a b c) '(a b c)) '()))
  (is (= (set-difference '(a b c d e) '(a c)) '(b d e)))
  (is (= (set-difference '(a b c d e) '(x y)) '(a b c d e))))

(defn intersection [a b]
  (filter #(member % b) a))

(deftest test-intersection
  (is (= (intersection '(a b) '(a b)) '(a b)))
  (is (= (intersection '(a b c) '(d e f)) '()))
  (is (= (intersection '(b c a) '(a d e)) '(a))))

(defn union [a b]
  (concat (set-difference a b) b))

(deftest test-union ()
  (is (= (union '(a b) '(a b)) '(a b)))
  (is (= (union '(a b c) '(d e f)) '(a b c d e f)))
  (is (= (union '(b c a) '(a d e)) '(b c a d e))))

;;;
;;;    7.17
;;;
(defn total-length [lol]
  (cond (empty? lol) 0
        :else (+ (count (first lol)) (total-length (rest lol)))) )

(defn total-length [lol]
  (reduce + (map count lol)))

(defn total-length [lol]
  (reduce (fn [n l] (+ n (count l))) 0 lol))

(deftest test-total-length
  (is (== (total-length '((a) (b c) (d) (e f g))) 7))
  (is (== (total-length '()) 0))
  (is (== (total-length '(() () () ())) 0))
  (is (== (total-length '(() (a) () (c d))) 3)))

;;;
;;;    7.19
;;;
(defn all-odd? [ints]
  (every? odd? ints))

(deftest test-all-odd?
  (is (all-odd? '(1 3 5 7)))
  (is (not (all-odd? '(1 2 3 4))))
  (is (all-odd? '())))

;;;
;;;    7.20
;;;
(defn none-odd? [ints]
  (not-any? odd? ints))

(deftest test-none-odd?
  (is (none-odd? '(2 4 6 8)))
  (is (not (none-odd? '(1 2 3 4))))
  (is (none-odd? '())))

;;;
;;;    7.21
;;;
(defn not-all-odd? [ints]
  (some even? ints))

(defn not-all-odd? [ints]
  (not-every? odd? ints))

(deftest test-not-all-odd?
  (is (not-all-odd? '(1 2 3 4)))
  (is (not-all-odd? '(2 4 6 8)))
  (is (not (not-all-odd? '(1 3 5 7))))
  (is (not (not-all-odd? '()))) )

;;;
;;;    7.22
;;;
(defn not-none-odd? [ints]
  (some odd? ints))

(deftest test-not-none-odd?
  (is (not-none-odd? '(1 2 3 4)))
  (is (not-none-odd? '(1 3 5 7)))
  (is (not (not-none-odd? '(2 4 6 8)))) ; Contains no odd elts
  (is (not (not-none-odd? '()))) ) ; Contains no odd elts

;;;
;;;    7.26
;;;
(defn find-if [f l]
  (let [l1 (filter f l)]
    (if (empty? l1)
      nil
      (first l1))))

(deftest test-find-if
  (is (= (find-if even? '(1 3 5 6 9)) 6))
  (is (= (find-if #(> % 5) '(2 4 4.1 4.2 6 8)) 6))
  (is (nil? (find-if odd? '(2 4 6 8)))) )

;;;
;;;    7.27
;;;
(defn every [f l]
  (empty? (remove f l)))

(deftest test-every
  (is (every even? '(2 4 6 8)))
  (is (not (every even? '(1 2 3 4 5))))
  (is (every string? '("Is" "this" "not" "pung?")))
  (is (not (every number? '(a b 2 3)))) )
