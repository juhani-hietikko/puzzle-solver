(ns puzzle-solver.pieces
  (:require [clojure.math.combinatorics :refer [cartesian-product]]))

(def pieces [{:shape      [0 0 :-
                           1 0 :|
                           2 0 :-
                           1 1 :-
                           1 2 :|]}
             {:shape      [0 0 :|
                           1 0 :-
                           0 1 :-
                           0 2 :|
                           -1 1 :|]}
             {:symmetric? true
              :shape      [0 0 :|
                           0 1 :-
                           0 2 :|
                           -1 1 :|
                           1 1 :|]}
             {:shape      [0 0 :|
                           0 1 :-
                           0 2 :|
                           1 0 :-
                           2 0 :|]}
             {:symmetric? true
              :shape      [0 0 :|
                           1 0 :-
                           2 0 :|
                           3 0 :-
                           4 0 :|]}
             {:shape      [0 0 :|
                           1 0 :-
                           2 0 :|
                           0 1 :-
                           2 1 :-]}
             {:shape      [0 0 :|
                           1 0 :-
                           1 1 :|
                           1 2 :-
                           1 3 :|]}
             {:shape      [0 0 :-
                           0 1 :|
                           0 2 :-
                           1 2 :|
                           1 3 :-]}
             {:shape      [0 0 :|
                           1 0 :-
                           0 1 :-
                           -1 1 :|
                           -1 2 :-]}
             {:shape      [0 0 :-
                           0 1 :|
                           1 1 :-
                           0 2 :-
                           0 3 :|]}
             {:shape      [0 0 :-
                           1 0 :|
                           0 1 :|
                           1 1 :-
                           1 2 :|]}
             {:shape      [0 0 :|
                           1 0 :-
                           1 1 :|
                           1 2 :-
                           2 2 :|]}])

(defn print-piece [p]
  (let [parts (partition 3 (:shape p))
        part-with-coords (fn [x y]
                           (some (fn [[xp yp s]] (when (and (= x xp) (= y yp)) s)) parts))]
    (println "")
    (println "")
    (doseq [y (reverse (range -3 5))]
      (prn (for [x (range -3 5)]
             (if-let [part (part-with-coords x y)]
               part
               10)))
      (println ""))))

(defn print-pieces []
  (doseq [p pieces]
    (print-piece p)))

(defn inverse [part]
  (if (= :| part)
    :-
    :|))

(defn- rotate-part [r [x y p]]
  (case r
    0 [x y p]
    1 [y (- x) (inverse p)]
    2 [(- x) (- y) p]
    3 [(- y) x (inverse p)]))

(defn parts [{:keys [shape]}]
  (partition 3 shape))

(defn- rotate-piece [piece r]
  (let [parts-rotated (map (partial rotate-part r) (parts piece))]
    {:shape (flatten parts-rotated)}))

(defn- rotations [piece]
  (for [r (range 0 (if (:symmetric? piece) 2 4))]
    (rotate-piece piece r)))

(def pieces-rotations (map rotations pieces))

; test finding a solution:
;(concat (repeat 3 '({:shape [0 0 :| 0 1 :- 0 2 :| 0 3 :- 0 4 :| 0 5 :- 0 6 :| 0 7 :-]}))
;        (repeat 3 '({:shape [0 0 :- 0 1 :| 0 2 :- 0 3 :| 0 4 :- 0 5 :| 0 6 :- 0 7 :|]}))
;        (repeat 2 '({:shape [0 0 :| 0 1 :- 0 2 :|]}))
;        (repeat 2 '({:shape [0 0 :- 0 1 :| 0 2 :-]})))

(defn print-rotations [piece]
  (doseq [r (range 0 4)]
    (print-piece (rotate-piece piece r))))

(defn piece-configs [all-rotations] (apply cartesian-product all-rotations))
