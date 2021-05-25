;;;;
;;;;
;;;;   In Clojure, because the language is so bendable, you actually bend language towards the problem, not the problem towards the language.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               transpose.clj
;;;;
;;;;   Started:            Fri Apr 16 12:03:41 2021
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

(ns transpose
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

(def notes [:c :c-sharp :d :d-sharp :e :f :f-sharp :g :g-sharp :a :a-sharp :b])
(def note-table (into {} (map-indexed #(vector %2 (inc %1)) notes)))
(def inverse-note-table (into {} (map-indexed #(vector (inc %1) %2) notes)))

;; (defn numbers
;;   "Convert `notes` into the corresponding sequence of number."
;;   [notes]
;;   (map #(% note-table) notes))

(defn note->number [note]
  (note note-table))

(defn numbers
  "Convert `notes` into the corresponding sequence of number."
  [notes]
  (map note->number notes))

(deftest test-numbers
  (is (= (numbers '(:e :d :c :d :e :e :e)) '(5 3 1 3 5 5 5))))

;; (defn notes
;;   "Convert `numbers` into the corresponding sequence of notes."
;;   [numbers]
;;   (map (fn [n] (first (first (filter (fn [[k v]] (= v n)) note-table)))) numbers))

;; (defn number->note [n]
;;   (first (first (filter (fn [[k v]] (= v n)) note-table))))

(defn number->note [n]
  (inverse-note-table n))

(defn notes
  "Convert `numbers` into the corresponding sequence of notes."
  [numbers]
  (map number->note numbers))

(deftest test-notes
  (is (= (notes '(5 3 1 3 5 5 5)) '(:e :d :c :d :e :e :e))))

(defn raise [shift notes]  ; ??? Really numbers...
  (map #(+ % shift) notes))

(deftest test-raise
  (is (= (raise 5 '(5 3 1 3 5 5 5)) '(10 8 6 8 10 10 10))))

;; (defn normalize [notes]
;;   (map #(mod % 12) notes))

;; (deftest test-normalize
;;   (= (normalize '(6 10 13)) '(6 10 1)))

;;;
;;;    The above is not quite good enough! 12 => 0
;;;    
(defn normalize [notes]
  (map #(inc (mod (+ % 11) 12)) notes))

(deftest test-normalize ()
  (= (normalize '(1 2 3 4 5 10 11 12)) '(1 2 3 4 5 10 11 12))
  (= (normalize '(6 10 13)) '(6 10 1))
  (= (normalize '(0 -1 -2 -3 -10 -11)) '(12 11 10 9 2 1)))

(defn transpose [n song]
  (notes (normalize (raise n (numbers song)))) )

(deftest test-transpose
  (is (= (transpose 5 '(:e :d :c :d :e :e :e)) '(:a :g :f :g :a :a :a))))
