;;;;
;;;;
;;;;   Clojure is great for apps where you need access to the bare meta.
;;;;   -- Jay Fields
;;;;
;;;;   Name:               family_record.clj
;;;;
;;;;   Started:            Sat Aug 28 12:08:57 2021
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
;;;;   Person records hold references to other Person records rather than just names of people.
;;;;   This causes a chicken and egg problem that requires the `children` keys to be populated
;;;;   in a second pass after instantiating everyone. It also forces the `defamily` macro to
;;;;   be ordered from ancestors -> descendants.
;;;;   Whoops... No can do. This is actually impossible in FP. It is a simple matter in imperative
;;;;   programming to create parent and child objects independently and then modify their slots to
;;;;   refer to each other later. However, without destructive modification, both the parent(s) and
;;;;   (potentially multiple) children would have to be created at the outset referring to each other.
;;;;   But since one must be instantiated before the other this is impossible.
;;;;
;;;;

(ns family-record
  (:use clojure.test
        clojure.set
        [clojure.pprint :only (cl-format)])
  (:import))

(defrecord Person [name father mother children])

;; (defmethod name ((o null)) nil)
;; (defmethod father ((o null)) nil)
;; (defmethod mother ((o null)) nil)
;; (defmethod children ((o null)) nil)
  
;; (defmethod print-object ((p person) stream)
;;   (print-unreadable-object (p stream :type t)
;;     (format stream "~A F: ~A M: ~A" (name p) (name (father p)) (name (mother p)))
;;     (when (children p)
;;       (format stream " Children: ~A" (mapcar #'name (children p)))) ))

;; (defn find-person [family name]
;;   (family name))

;;;
;;;    References in Person records simply as strings referring to other people:
;;;    E.g., father -> "Arthur"
;;;    
(defn add-child [parent child]
(println parent child)
  (if (nil? parent)
    parent
    (update parent :children conj child)))

(defn set-children [family]
  (reduce (fn [f [name p]]
            (let [father (add-child (f (:father p)) (:name p))
                  mother (add-child (f (:mother p)) (:name p))]
              (-> f
                  (assoc (:name father) father)
                  (assoc (:name mother) mother))))
          family
          family))

(defn update-family [family & members]
  (loop [f family
         m members]
    (cond (empty? m) f
          (nil? (first m)) (recur f (rest m))
          :else (recur (assoc f (:name (first m)) (first m)) (rest m)))) )
    
(defn set-children [family]
  (reduce-kv (fn [f name child]
               (let [father (add-child (f (:father child)) (:name child))
                     mother (add-child (f (:mother child)) (:name child))]
                 (update-family f father mother)))
             family
             family))

(defmacro defamily [name members]
  (let [family (reduce (fn [family [name father-name mother-name]]
                         (assoc family name (Person. name father-name mother-name [])))
                       {}
                       members)]
    `(def ~name ~(set-children family))))

(defn make-person
  ([name father mother] (make-person name father mother []))
  ([name father mother children] (Person. name father mother children)))

(defmacro defamily [name members]
  (let [family (apply update-family {} (map #(apply make-person %) members))]
    `(def ~name ~(set-children family))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    This can't work!
;;;
;;;    References as Person records:
;;;    E.g., father => #family_record.Person{:name "Arthur", :father nil, :mother nil, :children ["Bruce" "Ellen" "Charles" "David"]}
;;;    
;; (defn add-child [parent child]
;;  (println parent child)
;;   (if (nil? parent)
;;     parent
;;     (update parent :children conj child)))

;; (defn update-family [family & members]
;;   (loop [f family
;;          m members]
;;     (cond (empty? m) f
;;           (nil? (first m)) (recur f (rest m))
;;           :else (recur (assoc f (:name (first m)) (first m)) (rest m)))) )
    
;; (defn set-children [family]
;;   (reduce-kv (fn [f name child]
;;                (let [father (add-child (:father child) child)
;;                      mother (add-child (:mother child) child)
;;                      child (-> child (assoc :father father) (assoc :mother mother))]
;;                  (update-family f father mother)))
;;              family
;;              family))

;; (defmacro defamily [name members]
;;   (let [family (reduce (fn [family [name father-name mother-name]]
;;                          (assoc family name (Person. name (family father-name) (family mother-name) [])))
;;                        {}
;;                        members)]
;;     `(def ~name ~(set-children family))))

;; (defn make-person
;;   ([name father mother] (make-person name father mother []))
;;   ([name father mother children] (Person. name father mother children)))

;; (defmacro defamily [name members]
;;   (let [family (apply update-family {} (map #(apply make-person %) members))]
;;     `(def ~name ~(set-children family))))

;;;
;;;    Must be ordered from ancestors -> descendants
;;;    
(defamily the-family (("Colin" nil nil)
                      ("Deirdre" nil nil)
                      ("Arthur" nil nil)
                      ("Kate" nil nil)
                      ("Frank" nil nil)
                      ("Linda" nil nil)
                      ("Suzanne" "Colin" "Deirdre")
                      ("Bruce" "Arthur" "Kate")
                      ("Charles" "Arthur" "Kate")
                      ("David" "Arthur" "Kate")
                      ("Ellen" "Arthur" "Kate")
                      ("George" "Frank" "Linda")
                      ("Hillary" "Frank" "Linda")
                      ("Andre" nil nil)
                      ("Tamara" "Bruce" "Suzanne")
                      ("Vincent" "Bruce" "Suzanne")
                      ("Wanda" nil nil)
                      ("Ivan" "George" "Ellen")
                      ("Julie" "George" "Ellen")
                      ("Marie" "George" "Ellen")
                      ("Nigel" "Andre" "Hillary")
                      ("Frederick" nil "Tamara")
                      ("Zelda" "Vincent" "Wanda")
                      ("Joshua" "Ivan" "Wanda")
                      ("Quentin" nil nil)
                      ("Robert" "Quentin" "Julie")
                      ("Olivia" "Nigel" "Marie")
                      ("Peter" "Nigel" "Marie")
                      ("Erica" nil nil)
                      ("Yvette" "Robert" "Zelda")
                      ("Diane" "Peter" "Erica")))



;; (defn father [person family]
;;   (:father (family (keyword person))))

;; ;;;
;; ;;;    Works normally, but fails if `person` is nil.
;; ;;;    
;; ;; (defn father [person family]
;; ;;   (:father ((keyword person) family)))

;; (defn mother [person family]
;;   (:mother (family (keyword person))))

;; (defn father? [parent child family]
;;   (and (not (nil? parent))
;;        (not (nil? child))
;;        (= (father child family) parent)))

;; (defn mother? [parent child family]
;;   (and (not (nil? parent))
;;        (not (nil? child))
;;        (= (mother child family) parent)))

(deftest test-father
  (is (nil? (father nil the-family)))
  (is (not (father? nil nil the-family)))
  (is (father? (father 'yvette the-family) 'yvette the-family))
  (is (father? (father 'diane the-family) 'diane the-family))
  (is (father? (father 'suzanne the-family) 'suzanne the-family))
  (is (nil? (father 'frederick the-family))))

(deftest test-mother
  (is (nil? (mother nil the-family)))
  (is (not (mother? nil nil the-family)))
  (is (mother? (mother 'yvette the-family) 'yvette the-family))
  (is (mother? (mother 'diane the-family) 'diane the-family))
  (is (mother? (mother 'suzanne the-family) 'suzanne the-family))
  (is (nil? (mother 'colin the-family))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn parents [person family]
;;   (let [folks (remove nil? (list (father person family)
;;                                  (mother person family)))]
;;     (if (empty? folks)
;;       nil
;;       (set folks))))

(defn parents [person family]
  (set (remove nil? (list (father person family)
                          (mother person family)))) )

(deftest test-parents
;  (is (nil? (parents nil the-family)))
  (is (empty? (parents nil the-family)))
  (is (= (parents 'yvette the-family) '#{robert zelda}))
  (is (= (parents 'diane the-family) '#{peter erica}))
  (is (= (parents 'suzanne the-family) '#{colin deirdre}))
  (is (= (parents 'frederick the-family) '#{tamara}))
;  (is (nil? (parents 'colin the-family))))
  (is (empty? (parents 'colin the-family))))

(defn parent? [parent child family]
  (or (father? parent child family)
      (mother? parent child family)))

(defn child? [child parent family]
  (or (father? parent child family)
      (mother? parent child family)))

(defn child? [child parent family]
  (parent? parent child family))

(defn children [parent family]
  (if (nil? parent)
    #{}
    (reduce (fn [children person]
              (if (child? person parent family)
                (conj children (symbol person))
                children))
            #{}
            (keys family))))

(deftest test-children
  (is (empty? (children nil the-family)))
  (is (= (children 'arthur the-family) '#{charles bruce david ellen}))
  (is (every? #(child? % 'arthur the-family) (children 'arthur the-family))))

(defn siblings [person family]
  (let [fathers (disj (children (father person family) family) person)
        mothers (disj (children (mother person family) family) person)]
    (if (empty? fathers)
      mothers
      (if (empty? mothers)
        fathers
        (union fathers mothers)))) )

(defn siblings [person family]
  (let [fathers (disj (children (father person family) family) person)
        mothers (disj (children (mother person family) family) person)]
    (union fathers mothers)))

;;;
;;;    WTF?
;;;    
;; (deftest test-siblings
;;   (is (= (siblings 'david the-family) '#{david ellen charles bruce}))
;;   (is (= (siblings 'bruce the-family) '#{david ellen charles bruce}))
;;   (is (= (siblings 'marie the-family) '#{marie julie ivan}))
;;   (is (= (siblings 'zelda the-family) '#{joshua zelda})) ; Half-siblings
;;   (is (= (siblings 'joshua the-family) '#{joshua zelda})))

(deftest test-siblings
  (is (= (siblings 'david the-family) '#{ellen charles bruce}))
  (is (= (siblings 'bruce the-family) '#{david ellen charles}))
  (is (= (siblings 'marie the-family) '#{julie ivan}))
  (is (= (siblings 'zelda the-family) '#{joshua})) ; Half-siblings
  (is (= (siblings 'joshua the-family) '#{zelda})))

(defn mapunion [f a]
  (apply union (map #(set (f %)) a)))

(defn mapunion [f a]
  (reduce #(union %1 (set %2)) #{} (map f a)))

;; (union '#{a b} '(a c d))   D'oh!
;; (b a a c d)
;; (union '#{a b} '#{a c d})
;; #{a c b d}

(deftest test-mapunion
  (is (= (mapunion rest '((1 a b c) (2 e c j) (3 f a b c d))) '#{a e c j b d f})))

;;;
;;;    No right `partial` in Clojure...
;;;    
(defn grandparents [person family]
  (mapunion #(parents % family) (parents person family)))

(deftest test-grandparents
  (is (= (grandparents 'vincent the-family) '#{colin arthur deirdre kate}))
  (is (= (grandparents 'yvette the-family) '#{wanda julie quentin vincent}))
  (is (= (grandparents 'robert the-family) '#{ellen george}))
  (is (= (grandparents 'olivia the-family) '#{ellen andre hillary george}))
  (is (= (grandparents 'peter the-family) '#{ellen andre hillary george}))
  (is (= (grandparents 'diane the-family) '#{marie nigel})))

(defn cousins [person family]
  (mapunion #(children % family)
            (mapunion #(siblings % family)
                      (parents person family))))

(deftest test-cousins
  (is (= (cousins 'julie the-family) '#{nigel vincent tamara}))
  (is (= (cousins 'robert the-family) '#{peter olivia joshua}))
  (is (= (cousins 'ivan the-family) (cousins 'marie the-family) (cousins 'julie the-family)))
  (is (= (cousins 'olivia the-family) (cousins 'peter the-family))))

(defn descended-from? [descendant ancestor family]
  (let [folks (parents descendant family)]
    (cond (empty? folks) false
          (contains? folks ancestor) true
          :else (or (some #(descended-from? % ancestor family) folks) false))))

(defn descended-from? [descendant ancestor family]
  (if (parent? ancestor descendant family)
    true
    (or (some #(descended-from? % ancestor family) (parents descendant family)) false)))

;;;
;;;    The limited nature of the family tree necessitates that we check (nil? descendant)
;;;    to stop when we run out of parents. Otherwise we would create an inifinite loop.
;;;    There would be a different problem if we actually had an entirely accurate family tree!
;;;    If `descendant` were not descended from `ancestor`, the search would spin out of control
;;;    through antiquity (not quite infinite but still pointless).
;;;    
(defn descended-from? [descendant ancestor family]
  (if (nil? descendant)
    false
    (or (child? descendant ancestor family)
        (descended-from? (father descendant family) ancestor family)
        (descended-from? (mother descendant family) ancestor family))))

(deftest test-descended-from?
  (is (descended-from? 'yvette 'arthur the-family))
  (is (descended-from? 'yvette 'colin the-family))
  (is (descended-from? 'yvette 'deirdre the-family))
  (is (descended-from? 'yvette 'kate the-family))
  (is (descended-from? 'yvette 'frank the-family))
  (is (descended-from? 'yvette 'linda the-family))
  (is (not (descended-from? 'yvette 'andre the-family)))
  (is (descended-from? 'yvette 'suzanne the-family))
  (is (descended-from? 'yvette 'bruce the-family))
  (is (descended-from? 'yvette 'ellen the-family))
  (is (descended-from? 'yvette 'george the-family))
  (is (descended-from? 'tamara 'arthur the-family))
  (is (every? #(descended-from? 'yvette %) (ancestors 'yvette))))

(defn ancestors [person family]
  (cond (nil? person) #{}
        :else (union (parents person family)
                     (ancestors (father person family) family)
                     (ancestors (mother person family) family))) )

(deftest test-ancestors
  (is (= (ancestors 'marie the-family) '#{george ellen frank linda arthur kate}))
  (is (= (ancestors 'yvette the-family) '#{robert zelda quentin julie george ellen frank linda vincent wanda bruce suzanne arthur kate colin deirdre}))
  (is (= (ancestors 'diane the-family) '#{peter erica nigel marie andre hillary george ellen frank linda arthur kate})))

(defn generation-gap [descendant ancestor family]
  (letfn [(find-gap [descendant]
            (cond (nil? descendant) nil
                  (parent? ancestor descendant family) 1
                  :else (let [gap (some #(generation-gap % ancestor family) (parents descendant family))]
                          (if (nil? gap)
                            nil
                            (inc gap)))) )]
    (find-gap descendant)))

;;;
;;;    Touretzky has an interesting simplification.
;;;    (My modification.)
;;;
(defn generation-gap [descendant ancestor family]
  (letfn [(find-gap [descendant gap]
            (cond (nil? descendant) nil
                  (= descendant ancestor) gap
                  :else (or (find-gap (father descendant family) (inc gap))
                            (find-gap (mother descendant family) (inc gap)))) )]
    (find-gap descendant 0)))

;;;
;;;    Should be BFS!
;;;    Not as clear cut... Tree is not very deep. Even with exhaustive data, the tree would
;;;    be pretty shallow. 4000 years ≈ 200 generations
;;;
(defn generation-gap-bfs [descendant ancestor family]
  (letfn [(find-gap [ancestors]
            (if (empty? ancestors)
              nil
              (let [[a gap] (peek ancestors)
                    new-ancestors (pop ancestors)]
                (if (= a ancestor)
                  gap
                  (find-gap (reduce #(conj %1 [%2 (inc gap)]) new-ancestors (parents a family)))) )))]
    (find-gap (reduce #(conj %1 [%2 1]) clojure.lang.PersistentQueue/EMPTY (parents descendant family)))) )

(deftest test-generation-gap
  (is (== (generation-gap 'suzanne 'colin the-family) 1))
  (is (== (generation-gap 'frederick 'colin the-family) 3))
  (is (nil? (generation-gap 'frederick 'linda the-family)))
  (is (== (generation-gap 'olivia 'frank the-family) 3)))

