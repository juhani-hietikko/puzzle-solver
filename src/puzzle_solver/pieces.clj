(ns puzzle-solver.pieces
  (:require [clojure.math.combinatorics :refer [cartesian-product]]))

(def pieces [{:symmetric? true
              :shape      [0 0 :|
                           0 1 :-]}])

(defn inverse [part]
  )

(defn- rotate-part [r [x y p]]
  (case r
    0 [x y p]
    1 [y -x p]
    2 [-x y p]
    3 [-y x p]))

(defn- rotate-piece [{:keys [shape]} r]
  (let [parts (partition 3 shape)
        parts-rotated (map (partial rotate-part r) parts)]
    (flatten parts-rotated)))

(defn- rotations [piece]
  (for [r (range 0 (if (:symmetric? piece) 2 4))]
    (rotate-piece piece r)))

(def pieces-rotations (map rotations pieces))

(def piece-configs (cartesian-product pieces-rotations))
