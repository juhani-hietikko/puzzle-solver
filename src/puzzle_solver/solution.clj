(ns puzzle-solver.solution
  (:require [puzzle-solver.pieces :refer [pieces-rotations
                                          piece-configs
                                          parts]]
            [puzzle-solver.board :refer [empty-board
                                         available-placements
                                         place
                                         hopeless?]]))

(defn- solutions-in-branch [placed remaining board]
  (cond
    (empty? remaining)
    {:placements placed}
    (hopeless? board)
    nil
    :else
    (let [piece (first remaining)
          solutions (->> (available-placements board piece)
                         (map
                           #(solutions-in-branch
                              (cons {:piece  piece
                                     :coords %}
                                    placed)
                              (rest remaining)
                              (place board piece %)))
                         (filter identity)
                         flatten)]
      (when (seq solutions)
        solutions))))

(defn- potential-piece-config? [pc]
  (= 30 (count (->> pc
                    (mapcat parts)
                    (map #(nth % 2))
                    (filter #(= :- %))))))

(defn solutions-for-piece-config [pieces]
  (when (potential-piece-config? pieces)
    (solutions-in-branch '() pieces empty-board)))

(defn- rotation-with-possible-placements [piece-rotation]
  (assoc piece-rotation :possible-placements (available-placements empty-board piece-rotation)))

(defn- piece-with-possible-placements [piece-rotations]
  (map rotation-with-possible-placements piece-rotations))

(def pieces-rotations-with-possible-placements (map piece-with-possible-placements pieces-rotations))

(def all-piece-configs (piece-configs pieces-rotations-with-possible-placements))
