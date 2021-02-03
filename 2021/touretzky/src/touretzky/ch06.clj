;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               ch06.clj
;;;;
;;;;   Started:            Sun Jan 31 04:47:18 2021
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
  (:use (clojure test
                 set
                 [pprint :only (cl-format)])))

;;;
;;;    6.7
;;;    2013 version has a couple of interesting solutions.
;;;    
(defn next-to-last
  "Return the next-to-last element of a sequence."
  [s] (first (rest (reverse s))))

(defn next-to-last
  "Return the next-to-last element of a sequence."
  [s] (second (reverse s)))

(defn next-to-last
  "Return the next-to-last element of a sequence."
  [s] (nth s (- (count s) 2))) ; This is indadequate if fewer than 2 elts => IndexOutOfBoundsException

(deftest test-next-to-last
  (is (= (next-to-last '(a b c d e)) 'd))
  (is (= (next-to-last '(a b)) 'a))
  (is (= (next-to-last '(a b (c d) e)) '(c d))))

;;;
;;;    6.8
;;;
(defn butlast
  "Return a sequence except for the last element."
  [s] (reverse (rest (reverse s))))

(defn butlast
  "Return a sequence except for the last element."
  [s] (map (fn [a b] a) s (rest s)))

(defn butlast
  "Return a sequence except for the last element."
  [s] (cond (empty? (rest s)) '()
            :else (cons (first s) (butlast (rest s)))) )

(defn butlast
  "Return a sequence except for the last element."
  [s] (letfn [(collect [elt s]
                (if (empty? s)
                  '()
                  (cons elt (collect (first s) (rest s)))) )]
        (if (empty? s)
          '()
          (collect (first s) (rest s)))) )

(deftest test-butlast
  (is (= (butlast '(roses are red)) '(roses are)))
  (is (= (butlast '(g a g a)) '(g a g)))
  (is (= (butlast '(a)) '())))

;;;
;;;    6.10
;;;
(defn palindrome?
  "Is the given sequence `s` a palindrome?"
;  [s] (= s (reverse s)))
  [s] (= (seq s) (reverse s))) ; Have to do this to accomodate strings.

(deftest test-palindrome?
  (is (palindrome? '(a b c d c b a)))
  (is (not (palindrome? '(a b c a b c))))
  (is (palindrome? '[a b c d c b a]))
  (is (palindrome? "able was i ere i saw elba"))
  (is (not (palindrome? "race car"))))

;;;
;;;    6.11
;;;    
(defn make-palindrome
  "Make a palindrome out of the given sequence `s`"
  [s] (concat s (reverse s)))

;;;
;;;    Not ideal...
;;;    
;(make-palindrome "foo bar") => (\f \o \o \space \b \a \r \r \a \b \space \o \o \f)

(deftest test-make-palindrome
  (is (palindrome? (make-palindrome '(you and me))))
  (is (palindrome? (make-palindrome '[a b c d])))
  (is (palindrome? (make-palindrome "Is this not pung?"))))

;;;
;;;    Section 6.7
;;;    
(def titles '#{mr ms miss mrs})

(defn titled?
  "Does `name` begin with a title?"
  [name]
  (contains? titles (first name)))

(deftest test-titled?
  (is (titled? '(ms jane doe)))
  (is (not (titled? '(jane doe)))) )

(def male-first-names '#{john kim richard fred george})
(def female-first-names '#{jane mary wanda barbara kim})
(def gender-ambiguous-names (intersection male-first-names female-first-names))

(defn male?
  "Is `name` exclusively a male name?"
  [name]
  (and (contains? male-first-names name)
       (not (contains? female-first-names name))))

(defn female?
  "Is `name` exclusively a male name?"
  [name]
  (and (contains? female-first-names name)
       (not (contains? male-first-names name))))

(deftest test-male?
  (is (male? 'john))
  (is (male? 'richard))
  (is (not (male? 'kim)))
  (is (not (male? 'mary)))
  (is (not (male? 'fido))))

(deftest test-female?
  (is (female? 'jane))
  (is (female? 'wanda))
  (is (not (female? 'kim)))
  (is (not (female? 'fred)))
  (is (not (female? 'walmart))))

(defn give-title
  "Return a name with an appropriate title in front."
  [name]
  (cond (titled? name) name
        (male? (first name)) (cons 'mr name)
        (female? (first name)) (cons 'ms name)
        :else (list* 'mr 'or 'ms name)))

(deftest test-give-title
  (is (= (give-title '(miss jane adams)) '(miss jane adams)))
  (is (= (give-title '(john q public)) '(mr john q public)))
  (is (= (give-title '(barbara smith)) '(ms barbara smith)))
  (is (= (give-title '(kim johnson)) '(mr or ms kim johnson))))

;;;
;;;    6.26
;;;
(def ^:constant marker '-vs-)

(defn right-side
  "Return the features on the right side of the description."
  [description]
  (letfn [(traverse [descr]
            (cond (empty? descr) (throw (IllegalArgumentException. (cl-format false "Description is malformed: ~A" description)))
                  (= (first descr) marker) (rest descr)
                  :else (recur (rest descr))))]
    (traverse description)))

(deftest test-right-side
  (is (= (right-side '(large red shiny cube -vs- small shiny red four-sided pyramid)) '(small shiny red four-sided pyramid)))
  (is (thrown? IllegalArgumentException (right-side '(large red shiny cube small shiny red four-sided pyramid)))) )

(defn left-side
  "Return the features on the left side of the description."
  [description]
  (letfn [(traverse [descr]
            (cond (empty? descr) (throw (IllegalArgumentException. (cl-format false "Description is malformed: ~A" description)))
                  (= (first descr) marker) '()
                  :else (cons (first descr) (traverse (rest descr)))) )]
    (traverse description)))

(deftest test-left-side
  (is (= (left-side '(large red shiny cube -vs- small shiny red four-sided pyramid)) '(large red shiny cube)))
  (is (thrown? IllegalArgumentException (left-side '(large red shiny cube small shiny red four-sided pyramid)))) )

(defn count-common
  "How many features do the two descriptions have in common."
  [description]
  (count (intersection (set (left-side description)) (set (right-side description)))) )

(deftest test-count-common
  (is (== (count-common '(large red shiny cube -vs- small shiny red four-sided pyramid)) 2))
  (is (== (count-common '(large red shiny cube -vs- small shiny four-sided pyramid)) 1))
  (is (== (count-common '(large red cube -vs- small shiny four-sided pyramid)) 0)))

(defn compare
  "Report the number of features that two objects have in common."
  [description]
  (let [common (count-common description)]
    (case common
      1 (cons common '(common feature))
      (cons common '(common features)))) )

(deftest test-compare
  (is (= (compare '(large red shiny cube -vs- small blue four-sided pyramid)) '(0 common features)))
  (is (= (compare '(large red shiny cube -vs- small shiny four-sided pyramid)) '(1 common feature)))
  (is (= (compare '(large red shiny cube -vs- small shiny red four-sided pyramid)) '(2 common features)))
  (is (= (compare '(small red metal cube -vs- red plastic small cube)) '(3 common features))))

;;;
;;;    Section 6.9
;;;
(def things {:object1 '#{large green shiny cube}
             :object2 '#{small red dull metal cube}
             :object3 '#{red small dull plastic cube}
             :object4 '#{small dull blue metal cube}
             :object5 '#{small shiny red four-sided pyramid}
             :object6 '#{large shiny green sphere}})

;;;
;;;    
;;;    
(defn description
  "Description consists of a list of properties/features."
  [obj]
  (things obj))

(deftest test-description
  (is (= (description :object1) '#{large green shiny cube}))
  (is (nil? (description :foo))))

(defn symmetric-difference
  "Missing set operation, AKA, set-exclusive-or: (A-B)∪(B-A)"
  [a b]
  (union (difference a b) (difference b a)))

(defn differences
  "What properties are not shared by both objects?"
 [obj1 obj2]
  (symmetric-difference (description obj1)
                        (description obj2)))

(deftest test-differences
  (is (= (differences :object3 :object4) '#{red blue plastic metal}))
  (is (= (differences :object1 :object6) (differences :object6 :object1))))

(def properties '{size [large small]
                 color [red green blue]
                 luster [shiny dull]
                 material [metal plastic]
                 shape [cube sphere pyramid four-sided]})

(defn qualities-map
  "Extract qualities from properties."
  ([keys] (qualities-map keys {}))
  ([keys qualities]
   (if (empty? keys)
     qualities
     (let [[key & more] keys]
       (recur more (reduce (fn [result [k v]] (assoc result k v)) 
                           qualities
                           (map (fn [quality] [quality key]) (properties key)))) ))))
     
(def qualities (qualities-map (keys properties)))

(defn quality
  "Determine the quality of `property`"
  [property]
  (qualities property))

(deftest test-quality
  (is (= (quality 'red) 'color))
  (is (= (quality 'sphere) 'shape))
  (is (= (quality 'large) 'size))
  (is (= (quality 'dull) 'luster))
  (is (= (quality 'green) 'color))
  (is (= (quality 'metal) 'material))
  (is (= (quality 'sphere) 'shape)))

;; (defun quality-difference (obj1 obj2)
;;   (quality (first (differences obj1 obj2))))

;; (defun contrast (obj1 obj2)
;;   (remove-duplicates (sublis *quality-table* (differences obj1 obj2))))

;; ;;;
;; ;;;    MAPCAR is in ch7!!
;; ;;;    
;; (defun contrast (obj1 obj2)
;;   (remove-duplicates (mapcar #'quality (differences obj1 obj2))))

;; (deftest test-contrast ()
;;   (check
;;    (set-equal (contrast 'object3 'object4) '(color material))
;;    (null (contrast 'object5 'object5))))
