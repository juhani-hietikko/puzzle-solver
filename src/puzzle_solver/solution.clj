(ns puzzle-solver.solution
  (:require [puzzle-solver.pieces :refer [piece-configs]]
            [puzzle-solver.board :refer [empty-board
                                         possible-placements
                                         place
                                         hopeless?]]))
(def n (atom 0))

(defn- branch [placed remaining board]
  (swap! n inc)
  (cond
    #_(< (count remaining) 6)
    (empty? remaining)
    {:placements placed}
    (hopeless? board)
    nil
    #_(when (= 0 (mod @n 500))
        (prn board))
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

(defn solutions []
  ;(prn (count piece-configs))
  (branch '() (first piece-configs) empty-board))


#_(count (->> (take 1000 piece-configs)
              (filter (fn [pc]
                        (= 30 (count (->> pc
                                          (mapcat parts)
                                          (map #(nth % 2))
                                          (filter #(= :- %)))))))))