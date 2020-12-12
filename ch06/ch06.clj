;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch06.clj
;;;;
;;;;   Started:            Tue Aug 27 12:33:52 2013
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
;;;;   http://clojure.github.io/clojure/clojure.set-api.html
;;;;

(ns ch06
  (:use clojure.test
        clojure.set
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    6.6
;;;    Clojure's last function already does what the exercise asks.
;;;    Namely, (last l) == (cl:first (cl:last l))
;;;

;;;
;;;    6.7
;;;    
(defn next-to-last [seq]
  (second (reverse seq)))

(defn next-to-last [seq]
  (let [index (- (count seq) 2)]
    (if (neg? index)
      nil
      (nth seq index))))

(defn next-to-last [seq]
  (last (clojure.core/butlast seq)))

(defn next-to-last [seq]
  (letfn [(find-next-to-last [elt1 elt2 seq]
            (if (empty? seq)
              elt1
;              (find-next-to-last elt2 (first seq) (rest seq))))]
              (recur elt2 (first seq) (rest seq))))]
    (cond (empty? seq) nil
          (empty? (rest seq)) nil
          :else (find-next-to-last (first seq) (second seq) (rest (rest seq)))) ))

(deftest test-next-to-last
  (is (= (next-to-last '(a b c d)) 'c))
  (is (= (next-to-last '[a b]) 'a))
  (is (= (next-to-last "ab") \a))
  (is (= (next-to-last '(a)) nil))
  (is (= (next-to-last '()) nil)))

;;;
;;;    6.8
;;;
(defn butlast [seq]
  (reverse (rest (reverse seq))))

(defn butlast [seq]
  (cond (empty? seq) nil
        (empty? (rest seq)) nil
        :else (cons (first seq) (butlast (rest seq)))) )

(defn butlast [seq]
  (letfn [(collect [elt seq]
            (if (empty? seq)
              nil
              (cons elt (collect (first seq) (rest seq)))) )]
    (if (empty? seq)
      nil
      (collect (first seq) (rest seq)))) )

(defn butlast [seq]
  (map (fn [a b] a) seq (rest seq)))

(deftest test-butlast
  (is (= (butlast '(roses are red)) '(roses are)))
  (is (= (butlast '[g a g a]) '[g a g])))

;;;
;;;    6.9
;;;
(defn palindrome? [seq]
  (= seq (reverse seq))) ; Doesn't work for strings... (reverse "asdf") => (\f \d \s \a)

(defn palindrome? [seq]
  (every? true? (map = seq (reverse seq))))

(defn palindrome? [se]
  (= (seq se) (reverse se)))

(deftest test-palindrome?
  (is (palindrome? '[a b c d c b a]))
  (is (palindrome? "able was i ere i saw elba"))
  (is (not (palindrome? "race car"))))

;;;
;;;    6.10
;;;
(defn make-palindrome [seq]
  (concat seq (reverse seq)))

(deftest test-make-palindrome
  (is (palindrome? (make-palindrome '(you and me))))
  (is (palindrome? (make-palindrome '[a b c d])))
  (is (palindrome? (make-palindrome "Is this not pung?"))))

;;;
;;;    6.15
;;;
(defn contains-the? [sentence]
  (contains? (set sentence) 'the))

(defn contains-article? [sentence]
  (not (empty? (intersection '#{a an the} (set sentence)))) )

(defn contains-article? [sentence]
  (let [sentence (set sentence)]
    (or (contains? sentence 'the)
        (contains? sentence 'a)
        (contains? sentence 'an))))

(deftest test-contains-article?
  (is (contains-article? '(the quick brown fox jumps over the lazy dog)))
  (is (contains-article? '(the quick brown fox jumps over a lazy dog)))
  (is (contains-article? '(a quick brown fox jumps over an arthritic dog))))

