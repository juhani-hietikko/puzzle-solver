(ns puzzle-solver.core
  (:require [clojure.core.async :refer [chan
                                        onto-chan
                                        <!!
                                        go-loop]]
            [puzzle-solver.solution :refer [all-piece-configs
                                            solutions-for-one-piece-config]])
  (:gen-class))

(def thread-count 7)

(def running (atom false))
(def piece-configs-processed (atom 0))
(def solutions-found (atom '()))

(defn- process-piece-config [piece-config]
  (let [found-solutions (solutions-for-one-piece-config piece-config)]
    (swap! piece-configs-processed inc)
    (doseq [solution found-solutions]
      (println "FOUND A SOLUTION!!!")
      (println solution)
      (swap! solutions-found #(cons solution %)))))

(defn solve-starting-from [position]
  (println "Starting...")
  (reset! piece-configs-processed position)
  (reset! solutions-found '())
  (reset! running true)
  (let [piece-configs-chan (chan)]
    (onto-chan piece-configs-chan (drop position all-piece-configs))
    (dotimes [_ thread-count]
      (go-loop [pc (<!! piece-configs-chan)]
               (cond
                 (not (some? pc))
                 (println "Finished searching through all possible plays!")
                 (not @running)
                 (println "Stopping...")
                 :else
                 (do (process-piece-config pc)
                     (recur (<!! piece-configs-chan))))))))

(defn solve []
  (solve-starting-from 0))

(defn stop []
  (reset! running false))

(defn -main
  [& args]
  )
