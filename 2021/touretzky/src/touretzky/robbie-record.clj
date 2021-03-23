;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               robbie-record.clj
;;;;
;;;;   Started:            Wed Feb 24 19:21:49 2021
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
;;;;   Path is the sequence of locations.
;;;;   Moves are the directions followed to traverse the path.
;;;;
;;;;

(ns robbie-record
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

(defrecord Connection [source direction destination])

(defmacro defworld [name links]
  `(def ~name (reduce (fn [m# [source# _# & connections#]]
                        (into m# {source# (map (fn [[direction# destination#]] 
                                                 (Connection. source# direction# destination#))
                                               connections#)}))
                      {} '~links)))

(defworld house ((library -> (south back-stairs) (east upstairs-bedroom))
                 (back-stairs -> (north library) (south downstairs-bedroom))
                 (downstairs-bedroom -> (north back-stairs) (east dining-room))
                 (upstairs-bedroom -> (west library) (south front-stairs))
                 (front-stairs -> (north upstairs-bedroom) (south living-room))
                 (living-room -> (north front-stairs) (east kitchen) (south dining-room))
                 (dining-room -> (north living-room) (east pantry) (west downstairs-bedroom))
                 (kitchen -> (west living-room) (south pantry))
                 (pantry -> (north kitchen) (west dining-room))))

(defn warn [& args]
  (cl-format true "Warning: ")
  (apply cl-format true args))

(defn move-robbie
  "Move Robbie from `location` according to `moves` along a path defined by `world`."
  [location world moves]
  (cond (empty? moves) location
        :else (let [[move & more] moves
                    options (world location)]
                (if (nil? options)
                  (warn "Where are you Robbie?!~%")
                  (let [connection (first (drop-while (fn [conn] (not= (:direction conn) move)) options))]
                    (if (nil? connection)
                      (warn "Robbie cannot move ~A from ~A.~%" move location)
                      (recur (:destination connection) world more)))) )))

(deftest test-move-robbie
  (is (= (move-robbie 'pantry house '(north west)) 'living-room))
  (is (= (move-robbie 'pantry house '(north west north north west)) 'library))
  (is (= (move-robbie 'pantry house '(north west north north west south south east north)) 'living-room))
  (is (= (move-robbie 'upstairs-bedroom house '(south south south west north)) 'back-stairs))
  (is (= (move-robbie 'upstairs-bedroom house '(west south)) 'back-stairs)))

(defn random-move [moves]
  (nth moves (rand-int (count moves))))

(defn find-acyclic-move [visited options]
  (let [acyclic-moves (remove (comp (partial contains? visited) :destination) options)]
    (if (empty? acyclic-moves)
      (cl-format true "Robbie got stuck: ~A~%" visited)
      (:direction (random-move acyclic-moves)))) )

(defn execute-move [location direction world]
  (let [options (world location)
        connection (first (filter #(= (:direction %) direction) options))]
    (:destination connection)))

(defn trace-moves [start moves world]
  (loop [move (first moves)
         moves (rest moves)
         path [start]
         location (execute-move start move world)]
    (if (seq moves)
      (recur (first moves) (rest moves) (conj path '--> location) (execute-move location (first moves) world))
      (conj path '--> location))))

(defn find-path 
  "Find a random path through `world` from `start` to `finish`. Return the moves that traverse that path. Robbie gets stuck if he encounters a cycle."
  [start finish world]
  (letfn [(find-path-aux [location moves visited]
            (if (= location finish)
              moves
              (let [options (world location)
                    next-move (find-acyclic-move visited options)]
                (if (nil? next-move)
                  moves
                  (recur (execute-move location next-move world) (conj moves next-move) (conj visited location))))))]
    (if (= start finish)
        '()
        (let [{:keys [direction destination]} (random-move (world start))]
          (trace-moves start (find-path-aux destination [direction] #{start}) world)))) )
