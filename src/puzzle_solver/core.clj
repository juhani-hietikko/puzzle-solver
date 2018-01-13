(ns puzzle-solver.core
  (:require [clojure.core.async :refer [chan
                                        onto-chan
                                        <!!
                                        go-loop]]
            [puzzle-solver.solution :refer [all-piece-configs
                                            solutions-for-piece-config]]))

(def thread-count 7)

(def running? (atom false))
(def piece-configs-processed (atom 0))
(def solutions-found (atom '()))

(defn- process-piece-config [piece-config]
  (let [found-solutions (solutions-for-piece-config piece-config)]
    (swap! piece-configs-processed inc)
    (doseq [solution found-solutions]
      (println "FOUND A SOLUTION!!!")
      (println solution)
      (swap! solutions-found #(cons solution %)))))

(defn start-searching-from [start-index]
  (println "Starting...")
  (reset! piece-configs-processed start-index)
  (reset! solutions-found '())
  (reset! running? true)
  (let [piece-configs-chan (chan)]
    (onto-chan piece-configs-chan (drop start-index all-piece-configs))
    (dotimes [_ thread-count]
      (go-loop [next-piece-config (<!! piece-configs-chan)]
               (cond
                 (not next-piece-config)
                 (println "Finished searching through all possibilities!")
                 (not @running?)
                 (println "Stopping...")
                 :else
                 (do (process-piece-config next-piece-config)
                     (recur (<!! piece-configs-chan))))))))

(defn start-searching []
  (start-searching-from 0))

(defn stop-searching []
  (reset! running? false))
