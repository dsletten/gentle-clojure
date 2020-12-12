;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch06.clj
;;;;
;;;;   Started:            Sun Nov 24 22:51:00 2019
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

(ns ch06
  (:use clojure.test
        clojure.set
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    6.6
;;;    
(defn last-element [l]
  (if (empty? l)
    nil
    (last l)))

(deftest test-last-element
  (is (= (last-element '(a b c)) 'c))
  (is (= (last-element '(a b (c d))) '(c d)))
  (is (= (last-element '()) nil)))

(defn last-element-reverse [l]
  (first (reverse l)))

(deftest test-last-element-reverse
  (is (= (last-element-reverse '(a b c)) 'c))
  (is (= (last-element-reverse '(a b (c d))) '(c d)))
  (is (= (last-element-reverse '()) nil)))

(defn last-element-nth [l]
  (if (empty? l)
    nil
    (nth l (dec (count l)))) )

(deftest test-last-element-nth
  (is (= (last-element-nth '(a b c)) 'c))
  (is (= (last-element-nth '(a b (c d))) '(c d)))
  (is (= (last-element-nth '()) nil)))

;;;
;;;    6.7
;;;    
(defn next-to-last [l]
  (second (reverse l)))

(defn next-to-last [l]
  (letfn [(next-to-last-aux [l1 l2 l3]
             (if (empty? l3)
                 (first l1)
                 (recur (rest l1) (rest l2) (rest l3))))]
    (next-to-last-aux l (rest l) (rest (rest l)))) )

(deftest test-next-to-last
  (is (= (next-to-last '(a b c d e)) 'd))
  (is (= (next-to-last '(a b)) 'a))
  (is (= (next-to-last '(a b (c d) e)) '(c d))))

(defn next-to-last-nth [l]
  (nth l (- (count l) 2)))

(deftest test-next-to-last-nth
  (is (= (next-to-last-nth '(a b c d e)) 'd))
  (is (= (next-to-last-nth '(a b)) 'a))
  (is (= (next-to-last-nth '(a b (c d) e)) '(c d))))

;;;
;;;    6.8
;;;    
(defn butlast [l]
  (cond (empty? (rest l)) '()
        :else (cons (first l) (butlast (rest l)))) )

(defn butlast [l]
  (map (fn [a b] a) l (rest l)))

(defn butlast [l]
  (reverse (rest (reverse l))))

(defn butlast [l]
  (letfn [(butlast-aux [l result]
            (if (empty? (rest l))
              (reverse result)
              (recur (rest l) (cons (first l) result))))]
    (butlast-aux l '())))

(deftest test-butlast
  (is (= (butlast '(roses are red)) '(roses are)))
  (is (= (butlast '(g a g a)) '(g a g)))
  (is (= (butlast '(a)) '())))

;;;
;;;    6.10
;;;    
(defn palindrome? [l]
  (= l (reverse l))) ; Doesn't work for string...See 2013 solution.

(deftest test-palindrome?
  (is (palindrome? '(a b c d c b a)))
  (is (not (palindrome? '(a b c a b c)))) )

;;;
;;;    6.11
;;;    
(defn make-palindrome [l]
  (concat l (reverse l)))

(deftest test-make-palindrome
  (is (= (make-palindrome '(you and me)) '(you and me me and you)))
  (is (palindrome? (make-palindrome '(a b c)))) )

;;;
;;;    6.15
;;;    
(defn contains-article? [sentence]
  (not (empty? (intersection #{'a 'an 'the} sentence))))

(deftest test-contains-article?
  (is (contains-article? #{'sometimes 'a 'lonely 'way}))
  (is (contains-article? #{'and 'a 'new 'home 'for 'the 'trumpeter 'swan}))
  (is (not (contains-article? #{'we 'can 'see 'it 'now}))))

(defn contains-article? [sentence]
  (or (contains? sentence 'a)
      (contains? sentence 'an)
      (contains? sentence 'the)))

(defn contains-article? [sentence]
  (not (and (not (contains? sentence 'a))
            (not (contains? sentence 'an))
            (not (contains? sentence 'the)))) )

(defn contains-article? [sentence]
  (letfn [(contains-article-aux [articles]
            (cond (empty? articles) false
                  (contains? sentence (first articles)) true
                  :else (recur (rest articles))))]
    (contains-article-aux '(a an the))))

;;;
;;;    6.18
;;;
(def vowels #{'a 'e 'i 'o 'u})

(defn add-vowels [letters]
  (union letters vowels))

;;;
;;;    I.e., set equality...
;;;    
(defn validate [actual expected]
  (and (subset? actual expected)
       (subset? expected actual)))

(deftest test-add-vowels
  (is (validate (add-vowels #{'x 'a 'e 'z}) #{'x 'a 'e 'z 'i 'o 'u}))
  (is (validate (add-vowels #{}) #{'a 'e 'i 'o 'u}))
  (is (validate (add-vowels #{'a 'e 'i 'o 'u}) #{'a 'e 'i 'o 'u})))

;;;
;;;    Section 6.7
;;;    
(defn titled? [name]
  (contains? #{'mr 'ms 'miss 'mrs} (first name)))

(deftest test-titled?
  (is (titled? '(ms jane doe)))
  (is (not (titled? '(jane doe)))) )

(def male-first-names #{'john 'kim 'richard 'fred 'george})
(def female-first-names #{'jane 'mary 'wanda 'barbara 'kim})

(defn male? [name]
  (and (contains? male-first-names name)
       (not (contains? female-first-names name))))

(defn female? [name]
  (and (contains? female-first-names name)
       (not (contains? male-first-names name))))

(defn gender-ambiguous-names []
  (intersection male-first-names female-first-names))
(defn uniquely-male-names []
  (difference male-first-names female-first-names))
(defn uniquely-female-names []
  (difference female-first-names male-first-names))

;; (let ((male-first-names '(john kim richard fred george))
;;       (female-first-names '(jane mary wanda barbara kim)))
;;   (defun malep (name)
;;     (and (member name male-first-names)
;;          (not (member name female-first-names))))
;;   (defun femalep (name)
;;     (and (member name female-first-names)
;;          (not (member name male-first-names))))
;;   (defun gender-ambiguous-names ()
;;     (intersection male-first-names female-first-names))
;;   (defun uniquely-male-names ()
;;     (set-difference male-first-names female-first-names))
;;   (defun uniquely-female-names ()
;;     (set-difference female-first-names male-first-names)))

(deftest test-gender
  (is (male? 'richard))
  (is (not (male? 'barbara)))
  (is (female? 'barbara))
  (is (not (male? 'kim)))
  (is (not (female? 'kim))))

(defn give-title [name]
  (cond (titled? name) name
        (male? (first name)) (cons 'mr name)
        (female? (first name)) (cons 'ms name)
        :else (list* 'mr 'or 'ms name)))

(deftest test-give-title
  (is (= (give-title '(miss jane adams)) '(miss jane adams)))
  (is (= (give-title '(john q public)) '(mr john q public)))
  (is (= (give-title '(barbara smith)) '(ms barbara smith)))
  (is (= (give-title '(kim johnson)) '(mr or ms kim johnson))))


