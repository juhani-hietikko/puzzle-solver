(ns puzzle-solver.solution
  (:require [puzzle-solver.pieces :refer [piece-configs]]
            [puzzle-solver.board :refer [empty-board
                                         possible-placements
                                         place]]))

(defn- branch [placed remaining board]
  (swap! n inc)
  (if (empty? remaining)
    placed
    (let [piece (first remaining)
          possibilities (possible-placements board piece)]
      (some
        #(branch
           (cons {:piece  piece
                  :coords %}
                 placed)
           (rest remaining)
           (place board piece %))
        possibilities))))

(defn solutions []
  ;(prn (count piece-configs))
  (branch '() (first piece-configs) empty-board))
