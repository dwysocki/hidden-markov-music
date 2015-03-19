(ns hidden-markov-music.stats
  "General statistical functions."
  (:require [hidden-markov-music.util :refer [map-for]]))

(defn normalize
  "Normalizes a sequence."
  [sequence]
  (let [norm-constant (apply + sequence)]
    (map #(/ % norm-constant)
         sequence)))

(defn random-stochastic-vector
  "Returns a randomly generated stochastic vector of the given size."
  [size]
  (into [] (normalize (repeatedly size rand))))

(defn random-stochastic-map
  "Returns a hash-map representing a stochastic vector."
  [keys]
  (zipmap keys
          (random-stochastic-vector (count keys))))

(defn random-row-stochastic-map
  "Returns a nested hash-map representing a row-stochastic matrix.
  The outer key corresponds to a row, and the nested key corresponds to a
  column.

  For example, take the matrix `X` given by:

  ```
      0 1 2
    +-------+
  0 | a b c |
  1 | d e f |
  2 | g h i |
    +-------+
  ```

  To obtain the element `d`, one could use `(get-in X [1 0])`, or to obtain the
  element `h`, one could use `(get-in X [2 1])`."
  [row-keys col-keys]
  (let [n-cols (count col-keys)]
    (map-for [r row-keys]
             (let [col-probs (random-stochastic-vector n-cols)]
               (zipmap col-keys col-probs)))))
