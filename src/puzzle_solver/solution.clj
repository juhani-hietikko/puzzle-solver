(ns puzzle-solver.solution
  (:require [puzzle-solver.pieces :refer [pieces-rotations
                                          piece-configs
                                          parts]]
            [puzzle-solver.board :refer [empty-board
                                         possible-placements
                                         place
                                         hopeless?]]))

(defn- branch [placed remaining board]
  (cond
    (empty? remaining)
    {:placements placed}
    (hopeless? board)
    nil
    :else
    (let [piece (first remaining)
          solutions (->> (possible-placements board piece)
                         (map
                           #(branch
                              (cons {:piece  piece
                                     :coords %}
                                    placed)
                              (rest remaining)
                              (place board piece %)))
                         (filter identity)
                         flatten)]
      (when (seq solutions)
        solutions))))

(defn- rotation-with-limited-placements [piece-rotation]
  (assoc piece-rotation :possible-placements (possible-placements empty-board piece-rotation)))

(defn- piece-with-limited-placements [piece-rotations]
  (map rotation-with-limited-placements piece-rotations))

(def pieces-rotations-with-limited-placements (map piece-with-limited-placements pieces-rotations))

(defn potential-piece-config [pc]
  (= 30 (count (->> pc
                    (mapcat parts)
                    (map #(nth % 2))
                    (filter #(= :- %))))))

(defn- solutions-for-one-piece-config [pieces]
  (when (potential-piece-config pieces)
    (branch '() pieces empty-board)))

(defn my-pmap
  [f coll]
  (let [n 7
        rets (map #(future (f %)) coll)
        step (fn step [[x & xs :as vs] fs]
               (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
    (step rets (drop n rets))))

(defn solutions [pieces]
  (let [found-solutions (->> pieces
                             (my-pmap solutions-for-one-piece-config)
                             (filter identity))]
    (when (seq found-solutions)
      found-solutions)))

(def all-piece-configs (piece-configs pieces-rotations-with-limited-placements))
