(defproject puzzle-solver "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]]
  :main ^:skip-aot puzzle-solver.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
