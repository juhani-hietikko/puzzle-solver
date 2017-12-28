(ns puzzle-solver.board)

(def empty-board (for [x (range 0 8)]
                   (for [y (range 0 8)]
                     (cond
                       (and (#{3 4} x) (#{3 4} y))
                       :#
                       (= (mod x 2) (mod y 2))
                       :|
                       :else
                       :-))))
