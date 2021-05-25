;;;;
;;;;
;;;;   With Clojure we found that the very very low friction to get things done enables you to do things that you'd otherwise never even consider
;;;;   -- Orestis Markou
;;;;
;;;;   Name:               cards.clj
;;;;
;;;;   Started:            Sun Apr 18 19:58:59 2021
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
;;;;   Notes: Touretzky ex. 7.15
;;;;
;;;;

(ns cards
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

(defrecord Card [rank suit])

(def my-hand (map #(apply (fn [rank suit] (Card. rank suit)) %) '((3 :hearts) (5 :clubs) (2 :diamonds) (4 :diamonds) (:ace :spades))))

(defn count-suit [suit hand]
  (reduce (fn [count card]
            (if (= (:suit card) suit)
              (inc count)
              count))
          0
          hand))

(def colors {:clubs :black
             :diamonds :red
             :hearts :red
             :spades :black})

(defn color-of [card]
  (colors (:suit card)))

(deftest test-color-of
  (is (= (color-of (Card. 2 :clubs)) :black))
  (is (= (color-of (Card. 2 :hearts)) :red)))

(defn first-red [hand]
  (cond (empty? hand) nil
        (= (color-of (first hand)) :red) (first hand)
        :else (recur (rest hand))))

(defn black-cards [hand]
  (filter #(= (color-of  %) :black) hand))

(defn what-ranks [suit hand]
  (map :rank (filter #(= (:suit %) suit) hand)))

(def all-ranks (concat (range 2 11) [:jack :queen :king :ace]))

(defn before? [x y l & {:keys [test] :or {test =}}]
  (loop [result l]
    (cond (empty? result) false
          (test y (first result)) false
          (test x (first result)) result
          :else (recur (rest result)))) )

(defn higher-rank? [card1 card2]
  (if (before? (:rank card2) (:rank card1) all-ranks)
    true
    false))

(deftest test-higher-rank?
  (is (higher-rank? (Card. 10 :diamonds) (Card. 8 :spades)))
  (is (not (higher-rank? (Card. 10 :diamonds) (Card. :queen :spades)))) )

(defn best [f seq]
  "Return the first element as if the elements of SEQ were sorted by means of F."
  (reduce (fn [winner elt]
              (if (f elt winner)
                  elt
                  winner))
          seq))

;;;
;;;    Stable
;;;    
(defn high-card [hand]
  (reduce (fn [card1 card2] (if (higher-rank? card2 card1) card2 card1)) hand))

(defn high-card [hand]
  (best higher-rank? hand))

(deftest test-high-card
  (is (let [hand [(Card. 2 :spades)]] (= (high-card hand) (hand 0))))
  (is (let [hand [(Card. 2 :spades) (Card. :ace :spades) (Card. 8 :clubs) (Card. :queen :diamonds) (Card. :jack :clubs) (Card. 5 :hearts) (Card. 6 :spades)]] (= (high-card hand) (hand 1))))
  (is (let [hand [(Card. 5 :diamonds) (Card. 4 :hearts) (Card. :ace :clubs) (Card. 7 :spades) (Card. 8 :clubs) (Card. 4 :diamonds) (Card. 6 :diamonds)]] (= (high-card hand) (hand 2))))
  (is (let [hand [(Card. 5 :clubs) (Card. 3 :spades) (Card. 6 :spades) (Card. 6 :diamonds) (Card. :queen :clubs) (Card. :jack :spades) (Card. 5 :diamonds)]] (= (high-card hand) (hand 4))))
  (is (let [hand [(Card. 4 :clubs) (Card. 9 :spades) (Card. 6 :spades) (Card. :queen :clubs) (Card. :queen :diamonds) (Card. 2 :diamonds) (Card. 9 :spades)]] (= (high-card hand) (hand 3)))) ; Stable
  (is (let [hand [(Card. 2 :clubs) (Card. 3 :hearts) (Card. 5 :spades) (Card. 5 :spades) (Card. 9 :spades) (Card. 3 :spades) (Card. 9 :hearts)]] (= (high-card hand) (hand 4))))
  (is (let [hand [(Card. 4 :diamonds) (Card. 10 :hearts) (Card. 4 :diamonds) (Card. 10 :diamonds) (Card. :jack :spades) (Card. 8 :diamonds) (Card. :queen :diamonds)]] (= (high-card hand) (last hand))))
  (is (let [hand [(Card. :king :hearts) (Card. 10 :hearts) (Card. 6 :diamonds) (Card. 8 :hearts) (Card. 5 :diamonds) (Card. :jack :spades) (Card. :jack :spades)]] (= (high-card hand) (first hand)))) )
  
