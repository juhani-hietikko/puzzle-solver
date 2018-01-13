(ns puzzle-solver.board
  (:require [puzzle-solver.pieces :refer [parts]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.set :refer [intersection]]))

(def empty-board
  (let [on-center-piece? (fn [x y] (and (#{3 4} x) (#{3 4} y)))]
    {:squares       (vec (for [x (range 0 8)]
                           (vec (for [y (range 0 8)]
                                  (cond
                                    (on-center-piece? x y)
                                    :#
                                    (= (mod x 2) (mod y 2))
                                    :|
                                    :else
                                    :-)))))
     :empty-squares (->> (for [x (range 0 8)
                               y (range 0 8)]
                           (when-not (on-center-piece? x y)
                             [x y]))
                         (filter identity)
                         set)}))

(defn- square-empty? [{:keys [squares]} x y]
  (and (<= 0 x 7) (<= 0 y 7) (not= :# (get-in squares [x y]))))

(defn- part-fits? [{:keys [squares]} xo yo [xp yp p]]
  (let [x (+ xo xp)
        y (+ yo yp)]
    (and (<= 0 x 7) (<= 0 y 7) (= p (get-in squares [x y])))))

(defn can-place-in? [board piece [x y]]
  (every? (partial part-fits? board x y) (parts piece)))

(defn available-placements [{:keys [empty-squares] :as board} {:keys [possible-placements] :as piece}]
  (let [placements-to-check (if possible-placements
                              (intersection (set possible-placements) empty-squares)
                              empty-squares)]
    (filter #(can-place-in? board piece %) placements-to-check)))

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
  (every? (fn [[a b]]
            (not (square-empty? board a b)))
          (neighbours x y)))

(defn hopeless? [{:keys [empty-squares] :as board}]
  (some (partial isolated-empty-square? board) empty-squares))
