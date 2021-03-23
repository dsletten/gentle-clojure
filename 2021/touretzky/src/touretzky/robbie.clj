;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               robbie.clj
;;;;
;;;;   Started:            Tue Feb 16 15:13:28 2021
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

(ns robbie
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    I. Touretzky's way
;;;    
(def rooms {:living-room        {:north :front-stairs
                                 :east :kitchen
                                 :south :dining-room}
            :upstairs-bedroom   {:west :library
                                 :south :front-stairs}
            :dining-room        {:north :living-room
                                 :east :pantry
                                 :west :downstairs-bedroom}
            :kitchen            {:west :living-room
                                 :south :pantry}
            :pantry             {:north :kitchen
                                 :west :dining-room}
            :downstairs-bedroom {:north :back-stairs
                                 :east :dining-room}
            :back-stairs        {:north :library
                                 :south :downstairs-bedroom}
            :front-stairs       {:north :upstairs-bedroom
                                 :south :living-room}
            :library            {:south :back-stairs
                                 :east :upstairs-bedroom}})

(defn choices
  "What are the choices of direction from the given `room`?"
  [room]
  (rooms room))

(deftest test-choices
  (is (= (choices :pantry) {:north :kitchen, :west :dining-room}))
  (is (= (choices :living-room) {:north :front-stairs, :east :kitchen, :south :dining-room})))

(defn look
  "Where would Robbie end up if he moved in `direction` from `room`?"
  [direction room]
  ((choices room) direction))

(deftest test-look
  (is (= (look :north :pantry) :kitchen))
  (is (= (look :west :pantry) :dining-room))
  (is (nil? (look :south :pantry))))

(defonce location (atom :pantry))
(defn set-robbie-location
  "Update Robbie's location."
  [place]
  (reset! location place))

(defn how-many-choices
  "How many choices does Robbie have given his current location?"
  []
  (count (choices @location)))

(defn upstairs?
  "Is `place` upstairs?"
  [place]
  (#{:library :upstairs-bedroom} place))

(deftest test-upstairs?
  (is (upstairs? :library))
  (is (upstairs? :upstairs-bedroom))
  (is (not (upstairs? :downstairs-bedroom))))

(defn onstairs?
  "Is `place` on the stairs?"
  [place]
  (#{:back-stairs :front-stairs} place))

(deftest test-onstairs?
  (is (onstairs? :back-stairs))
  (is (onstairs? :front-stairs))
  (is (not (onstairs? :library))))

(defn where
  "Where is Robbie?"
  []
  (cond (upstairs? @location) (cl-format true "Robbie is upstairs in the ~A.~%" (name @location))
        (onstairs? @location) (cl-format true "Robbie is on the ~A.~%" (name @location))
        :else (cl-format true "Robbie is downstairs in the ~A.~%" (name @location))))

(defn move
  "Move Robbie in the given `direction` if possible."
  [direction]
  (let [new-location (look direction @location)]
    (if (nil? new-location)
      (cl-format true "Ouch! Robbie hit a wall.~%")
      (do (set-robbie-location new-location)
          (where)))) )

  
;;;
;;;    II. Map encoded as FSM (?) in local functions.
;;;
(defmacro dispatch [location [& destinations] otherwise]
  `(case ~location
     ~@(mapcat (fn [destination] (list destination (list (symbol destination)))) destinations)
     ~otherwise))

;; (macroexpand-1 '(dispatch current
;;                           (:library :back-stairs :upstairs-bedroom :front-stairs :living-room :kitchen :dining-room :downstairs-bedroom :pantry)
;;                           (warn "Where are you Robbie?!")))
;; (clojure.core/case current
;;   :library (library)
;;   :back-stairs (back-stairs)
;;   :upstairs-bedroom (upstairs-bedroom)
;;   :front-stairs (front-stairs)
;;   :living-room (living-room)
;;   :kitchen (kitchen)
;;   :dining-room (dining-room)
;;   :downstairs-bedroom (downstairs-bedroom)
;;   :pantry (pantry)
;;   (warn "Where are you Robbie?!"))

(defn warn [& args]
  (cl-format true "Warning: ")
  (apply cl-format true args))

(defn robbie-fsm [current direction]
  (letfn [(library []
            (case direction
              :south :back-stairs
              :east :upstairs-bedroom
              (illegal-move)))
          (back-stairs []
            (case direction
              :north :library
              :south :downstairs-bedroom
              (illegal-move)))
          (downstairs-bedroom []
            (case direction
              :north :back-stairs
              :east :dining-room
              (illegal-move)))
          (upstairs-bedroom []
            (case direction
              :west :library
              :south :front-stairs
              (illegal-move)))
          (front-stairs []
            (case direction
              :north :upstairs-bedroom
              :south :living-room
              (illegal-move)))
          (living-room []
            (case direction
              :north :front-stairs
              :east :kitchen
              :south :dining-room
              (illegal-move)))
          (dining-room []
            (case direction
              :north :living-room
              :east :pantry
              :west :downstairs-bedroom
              (illegal-move)))
          (kitchen []
            (case direction
              :west :living-room
              :south :pantry
              (illegal-move)))
          (pantry []
            (case direction
              :north :kitchen
              :west :dining-room
              (illegal-move)))
          (illegal-move []
            (warn "Robbie cannot move ~A from ~A." direction current))]
    (dispatch current
      (:library :back-stairs :upstairs-bedroom :front-stairs :living-room :kitchen :dining-room :downstairs-bedroom :pantry)
      (warn "Where are you Robbie?!"))))

(defn move-robbie [start moves]
  (cond (empty? moves) start
        :else (move-robbie (robbie-fsm start (first moves)) (rest moves))))

(deftest test-move-robbie
  (is (= (move-robbie :pantry [:north :west]) :living-room))
  (is (= (move-robbie :pantry [:north :west :north :north :west]) :library))
  (is (= (move-robbie :pantry [:north :west :north :north :west :south :south :east :north]) :living-room))
  (is (= (move-robbie :upstairs-bedroom [:south :south :south :west :north]) :back-stairs))
  (is (= (move-robbie :upstairs-bedroom [:west :south]) :back-stairs)))


