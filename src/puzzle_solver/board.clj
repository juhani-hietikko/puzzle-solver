(ns puzzle-solver.board
  (:require [puzzle-solver.pieces :refer [parts]]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(def empty-board (vec (for [x (range 0 8)]
                        (vec (for [y (range 0 8)]
                               (cond
                                 (and (#{3 4} x) (#{3 4} y))
                                 :#
                                 (= (mod x 2) (mod y 2))
                                 :|
                                 :else
                                 :-))))))

(defn- square-free? [board x y]
  (and (<= 0 x 7) (<= 0 y 7) (not= :# (get-in board [x y]))))

(defn- part-fits? [board xo yo [xp yp p]]
  (let [x (+ xo xp)
        y (+ yo yp)]
    (and (square-free? board x y) (= p (get-in board [x y])))))

(defn can-place-in? [board piece [x y]]
  (every? (partial part-fits? board x y) (parts piece)))

(def board-area (cartesian-product (range 0 8) (range 0 8)))

(defn possible-placements [board piece]
  (filter #(can-place-in? board piece %) board-area))

(defn- place-part [xo yo board [xp yp]]
  (let [x (+ xo xp)
        y (+ yo yp)]
    (assoc-in board [x y] :#)))

(defn place [board piece [x y]]
  (reduce (partial place-part x y) board (parts piece)))

(defn- neighbours [x y]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn isolated-empty-square? [board [x y]]
  (and (square-free? board x y)
      (every? (fn [[a b]] (not (square-free? board a b))) (neighbours x y))))

(defn impossible? [board]
  (some (partial isolated-empty-square? board) board-area))