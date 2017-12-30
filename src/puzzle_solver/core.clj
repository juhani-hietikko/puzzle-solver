(ns puzzle-solver.core
  (:require [puzzle-solver.solution :refer [all-piece-configs
                                            solutions]])
  (:gen-class))

(def running (atom false))
(def piece-configs-processed (atom 0))
(def solutions-found (atom '()))

(def mcount (memoize count))

(defn piece-configs-total []
  (mcount all-piece-configs))

(defn- solve-one-chunk-at-a-time [piece-configs-to-process]
  (let [this-chunk (take 1000 piece-configs-to-process)
        remaining (drop 1000 piece-configs-to-process)
        found-solutions (solutions this-chunk)]
    (swap! piece-configs-processed #(+ % (count this-chunk)))
    (doseq [solution found-solutions]
      (println "FOUND A SOLUTION!!!")
      (println solution)
      (swap! solutions-found #(cons solution %)))
    (cond
      (not @running)
      (println "Stopping...")
      (not (seq remaining))
      (println "Finished searching through all possible plays!")
      :else
      (recur remaining))))

(defn solve-starting-from [position]
  (println "Starting...")
  (reset! piece-configs-processed position)
  (reset! solutions-found '())
  (reset! running true)
  (future (solve-one-chunk-at-a-time (drop position all-piece-configs))))

(defn solve []
  (solve-starting-from 0))

(defn stop []
  (reset! running false))

(defn -main
  [& args]
  (solutions (take 1000 all-piece-configs)))
