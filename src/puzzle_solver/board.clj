(ns puzzle-solver.board
  (:require [puzzle-solver.pieces :refer [parts]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.set :refer [intersection]]))

(def empty-board
  {:squares       (vec (for [x (range 0 8)]
                         (vec (for [y (range 0 8)]
                                (cond
                                  (and (#{3 4} x) (#{3 4} y))
                                  :#
                                  (= (mod x 2) (mod y 2))
                                  :|
                                  :else
                                  :-)))))
   :empty-squares (->> (for [x (range 0 8)
                             y (range 0 8)]
                         (when-not (and (#{3 4} x) (#{3 4} y))
                           [x y]))
                       (filter identity)
                       set)})

(defn- square-free? [{:keys [squares]} x y]
  (and (<= 0 x 7) (<= 0 y 7) (not= :# (get-in squares [x y]))))

(defn- part-fits? [{:keys [squares] :as board} xo yo [xp yp p]]
  (let [x (+ xo xp)
        y (+ yo yp)]
    (and (<= 0 x 7) (<= 0 y 7) (= p (get-in squares [x y])))))

(defn can-place-in? [board piece [x y]]
  (every? (partial part-fits? board x y) (parts piece)))

(defn possible-placements [{:keys [empty-squares] :as board} {:keys [possible-placements] :as piece}]
  (let [limited-placements (if possible-placements
                             (intersection (set possible-placements) empty-squares)
                             empty-squares)]
    (filter #(can-place-in? board piece %) limited-placements)))

(defn- place-part [xo yo board [xp yp]]
  (let [x (+ xo xp)
        y (+ yo yp)]
    (-> board
        (assoc-in [:squares x y] :#)
        (update :empty-squares #(disj % [x y])))))

(defn place [board piece [x y]]
  (reduce (partial place-part x y) board (parts piece)))

(defn- neighbours [x y]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn isolated-empty-square? [board [x y]]
  (every? (fn [[a b]] (not (square-free? board a b))) (neighbours x y)))

(defn hopeless? [{:keys [empty-squares] :as board}]
  (some (partial isolated-empty-square? board) empty-squares))
