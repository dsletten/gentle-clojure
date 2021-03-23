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

;; (defn qualities-map
;;   "Extract qualities from properties."
;;   ([keys] (qualities-map keys {}))
;;   ([keys qualities]
;;    (if (empty? keys)
;;      qualities
;;      (let [[key & more] keys]
;;        (recur more (reduce (fn [result [k v]] (assoc result k v)) 
;;                            qualities
;;                            (map (fn [quality] [quality key]) (properties key)))) ))))

;; (defn qualities-map
;;   "Extract qualities from properties."
;;   ([keys] (qualities-map keys {}))
;;   ([keys qualities]
;;    (if (empty? keys)
;;      qualities
;;      (let [[key & more] keys]
;;        (recur more (into qualities (map (fn [quality] [quality key]) (properties key)))) ))))

(defn invert-map
  "Invert a map {key1 [val1 ... valn] ...} to {val1 key1 ... valn key1 ...}"
  [in]
  (reduce (fn [m [key vals]] (into m (map (fn [v] [v key]) (if (coll? vals) vals [vals])))) {} in))

(def qualities (invert-map properties))

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

(defn quality-difference
  "Determine one quality that differs between a pair of objects."
  [obj1 obj2] (quality (first (differences obj1 obj2))))

(defn contrast
  "Determine the qualities that differ between a pair of objects."
  [obj1 obj2]
  (set (map quality (differences obj1 obj2))))

(deftest test-contrast ()
  (is (= (contrast :object3 :object4) '#{color material}))
  (is (empty? (contrast :object5 :object5))))

;;;
;;;    6.30
;;;
(def books {"Before The Frost" "Henning Mankell"
            "Daemon" "Daniel Suarez"
            "Cat's Cradle" "Kurt Vonnegut"
            "Prince Caspian" "C. S. Lewis"
            "PAIP" "Peter Norvig"})

;;;
;;;    6.31
;;;
(defn who-wrote
  "Who is the author of `book`?"
  [book] (books book))

;;;
;;;    6.33
;;;
(def authors (invert-map books))

(defn wrote-what
  "Which books did `author` write?"
  [author] (authors author))

;;;
;;;    6.34
;;;
;; (def atlas [[:pennsylvania :pittsburgh]
;;             [:new-jersey :newark]
;;             [:pennsylvania :johnstown]
;;             [:ohio :columbus]
;;             [:new-jersey :princeton]
;;             [:new-jersey :trenton]])

(def atlas {:pennsylvania [:pittsburgh :johnstown]
            :new-jersey [:newark :princeton :trenton]
            :ohio [:columbus]})

(defn find-cities
  "Find all cities in the atlas located in `state`."
  [state]
  (atlas state))

(deftest test-find-cities
  (is (= (find-cities :pennsylvania) [:pittsburgh :johnstown]))
  (is (= (find-cities :new-jersey) [:newark :princeton :trenton]))
  (is (empty? (find-cities :pennsyltucky))))

;;;
;;;    6.35
;;;    
(def nerd-states {:sleeping :eating
                  :eating :waiting-for-a-computer
                  :waiting-for-a-computer :programming
                  :programming :debugging
                  :debugging :sleeping})

(defn nerdus
  "What is the nerd's next state following `state`?"
  [state] (nerd-states state))

(deftest test-nerdus
  (is (= (nerdus :programming) :debugging))
  (is (= (nerdus :sleeping) :eating))
  (is (nil? (nerdus :playing-guitar))))

(defn sleepless-nerd
  "What is the caffeinated nerd's next state following `state`?"
  [state]
  (let [next-state (nerdus state)]
    (case next-state
      :sleeping (nerdus next-state)
      next-state)))

(deftest test-sleepless-nerd
  (is (= (sleepless-nerd :eating) :waiting-for-a-computer))
  (is (= (sleepless-nerd :debugging) :eating))
  (is (= (sleepless-nerd :sleeping) :eating))) ; ???

(defn nerd-on-caffeine
  "What is the over-caffeinated nerd's next state following `state`?"
  [state] (nerdus (nerdus state)))
  
(deftest test-nerd-on-caffeine
  (is (= (nerd-on-caffeine :sleeping) :waiting-for-a-computer))
  (is (= (nerd-on-caffeine :eating) :programming))
  (is (= (nerd-on-caffeine :waiting-for-a-computer) :debugging))
  (is (= (nerd-on-caffeine :programming) :sleeping))
  (is (= (nerd-on-caffeine :debugging) :eating)))

(defmacro defchain
  [var vals & default]
  (let [chain (mapcat (fn [key val] `(~key '~val)) vals (concat (rest vals) (list (first vals))))]
    `(case ~var ~@chain ~@default)))

(defn nerdus [state]
  (defchain state (:sleeping :eating :waiting-for-a-computer :programming :debugging) nil))

;;;
;;;    6.36
;;;
(defn swap-first-last
  "Swap the first and last elements of a list."
  [l]
  (if (empty? l) 
    l
    (let [[elt0 & more] l]
      (if (empty? more)
        l
        (concat (list (last l)) (butlast more) (list elt0)))) ))

;; (defn swap-first-last
;;   "Swap the first and last elements of a list."
;;   [l]
;;   (letfn [(swap [l elt0 eltn result]
;;             (cond (empty? l) (cons eltn (reverse (cons elt0 result)))
;;                   :else (recur (rest l) elt0 (first l) (cons eltn result))))]
;;     (cond (empty? l) l
;;           (empty? (rest l)) l
;;           :else (swap (nthrest l 2) (first l) (second l) '()))) )

(defn swap-first-last
  "Swap the first and last elements of a list."
  [l]
  (letfn [(swap [l elt0 eltn result]
            (cond (empty? l) (cons eltn (reverse (cons elt0 result)))
                  :else (recur (rest l) elt0 (first l) (cons eltn result))))]
    (if (empty? l) 
      l
      (let [[elt0 & more] l]
        (if (empty? more)
          l
          (swap (rest more) elt0 (first more) '()))) )))

(defn swap-first-last
  "Swap the first and last elements of a list."
  [l]
  (letfn [(swap [elt0 reversed]
            (cons (first reversed) (reverse (cons elt0 (rest reversed)))) )]
    (if (empty? l) 
      l
      (let [[elt0 & more] l]
        (if (empty? more)
          l
          (swap elt0 (reverse more)))) )))

(defn swap-first-last
  "Swap the first and last elements of a list."
  [l]
  (letfn [(swap [elt0 more result]
            (cond (empty? more) (cons (first result) (reverse (cons elt0 (rest result))))
                  :else (swap elt0 (rest more) (cons (first more) result))))]
    (if (empty? l) 
      l
      (let [[elt0 & more] l]
        (if (empty? more)
          l
          (swap elt0 more '()))) )))

;; (defn swap-first-last
;;   "Swap the first and last elements of a list."
;;   [l]
;;   (letfn [(swap [l elt0 eltn result]
;;             (cond (empty? l) (cons eltn (reverse (cons elt0 result)))
;;                   :else (recur (rest l) elt0 (first l) (cons eltn result))))]
;;     (cond (empty? l) l
;;           (empty? (rest l)) l
;;           :else (swap (nthrest l 2) (first l) (second l) '()))) )

;; (defn swap-first-last
;;   "Swap the first and last elements of a list."
;;   [l]
;;   (letfn [(swap [l elt0 eltn result]
;;             (cond (empty? l) (cons eltn (reverse (cons elt0 result)))
;;                   :else (recur (rest l) elt0 (first l) (cons eltn result))))]
;;     (if (empty? l) 
;;       l
;;       (let [[elt0 & more] l]
;;         (if (empty? more)
;;           l
;;           (swap (rest more) elt0 (first more) '()))) )))

(defn swap-first-last
  "Swap the first and last elements of a list."
  [l]
  (letfn [(swap [l a z result]
            (cond (empty? l) (cons z (reverse (cons a result)))
                  :else (recur (rest l) a (first l) (cons z result))))]
    (if (< (count l) 2) ; Inexpensive in Clojure
      l
      (swap (nthrest l 2) (first l) (second l) '()))) )

(deftest test-swap-first-last
  (is (= (swap-first-last '()) '()))
  (is (= (swap-first-last '(a)) '(a)))
  (is (= (swap-first-last '(a b)) '(b a)))
  (is (= (swap-first-last '(you cant buy love)) '(love cant buy you))))

;;;
;;;    6.37
;;;
(defn rotate-left
  "Rotate the elements of a list to the left. The first becomes the last."
  [l]
  (case (count l)
    (0 1) l
    (concat (rest l) (list (first l)))) )

(defn rotate-left
  "Rotate the elements of a list to the left. The first becomes the last."
  [l]
  (letfn [(rotate [a l]
            (cond (empty? l) (list a)
                  :else (cons (first l) (rotate a (rest l)))) )]
    (if (< (count l) 2)
      l
      (rotate (first l) (rest l)))) )

;;;
;;;    Tail recursive.
;;;    
(defn rotate-left
  "Rotate the elements of a list to the left. The first becomes the last."
  [l]
  (letfn [(rotate [a l result]
            (cond (empty? l) (reverse (cons a result))
                  :else (recur a (rest l) (cons (first l) result))))]
    (if (< (count l) 2)
      l
      (rotate (first l) (rest l) '()))) )

(deftest test-rotate-left
  (is (= (rotate-left '()) '()))
  (is (= (rotate-left '(a)) '(a)))
  (is (= (rotate-left '(a b)) '(b a)))
  (is (= (rotate-left [[:a 1] [:b 2] [:c 3]]) [[:b 2] [:c 3] [:a 1]]))
  (is (= (rotate-left (rotate-left (rotate-left '(a b c)))) '(a b c)))
  (is (= (rotate-left '(a b c d e)) '(b c d e a))))

(defn rotate-right
  "Rotate the elements of a list to the right. The last becomes the first."
  [l]
  (case (count l)
    (0 1) l
    (concat (list (last l)) (butlast l))))

;; (defn rotate-right
;;   "Rotate the elements of a list to the right. The last becomes the first."
;;   [l]
;;   (letfn [(rotate [z l result]
;;             (cond (empty? l) (cons z (reverse result))
;;                   :else (recur (first l) (rest l) (cons z result))))]
;;     (if (< (count l) 2)
;;       l
;;       (rotate (second l) (nthrest l 2) (list (first l)))) ))

(defn rotate-right
  "Rotate the elements of a list to the right. The last becomes the first."
  [l]
  (letfn [(rotate [previous l result]
            (cond (empty? l) (cons previous (reverse result))
                  :else (recur (first l) (rest l) (cons previous result))))]
    (if (< (count l) 2)
      l
      (rotate (first l) (rest l) '()))) )

(deftest test-rotate-right
  (is (= (rotate-right '()) '()))
  (is (= (rotate-right '(a)) '(a)))
  (is (= (rotate-right '(a b)) '(b a)))
  (is (= (rotate-right (rotate-right (rotate-right '(a b c)))) '(a b c)))
  (is (= (rotate-right '(a b c d e)) '(e a b c d))))

