(ns hidden-markov-music.stats
  "General statistical functions."
  (:require [hidden-markov-music.math :refer [log exp log-sum mean stdev]]
            [hidden-markov-music.util :refer [map-for
                                              numbers-almost-equal?]]))

(defn positive-outliers
  "Filters out all elements from `coll` except for those more than `sigma`
  standard deviations *above* the mean."
  [coll sigma]
  (let [mean  (mean  coll)
        stdev (stdev coll)]
    (filter (fn [x]
              (> (/ (- x mean)
                    stdev)
                 sigma))
            coll)))

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

(defn uniform-stochastic-vector
  "Returns a uniformly distributed stochastic vector of the given size."
  [size]
  (into [] (repeat size (/ 1 size))))

(defn random-stochastic-map
  "Returns a hash-map representing a randomly distributed stochastic vector."
  [keys]
  (zipmap keys
          (random-stochastic-vector (count keys))))

(defn uniform-stochastic-map
  "Returns a hash-map representing a uniformly distributed stochastic vector."
  [keys]
  (zipmap keys
          (uniform-stochastic-vector (count keys))))


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

(defn uniform-row-stochastic-map
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
             (let [col-probs (uniform-stochastic-vector n-cols)]
               (zipmap col-keys col-probs)))))

(defn stochastic-map?
  "Returns true if the map is stochastic to the given precision."
  [m & {:keys [decimal] :or {decimal 10}}]
  (numbers-almost-equal? (reduce + (vals m))
                         1.0
                         :decimal decimal))

(defn log-stochastic-map?
  "Returns true if the map is logarithmically stochastic to the given
  precision."
  [m & {:keys [decimal] :or {decimal 10}}]
  (numbers-almost-equal? (reduce log-sum (vals m))
                         0.0
                         :decimal decimal))

(defn row-stochastic-map?
  "Returns true if the map is row stochastic to the given precision."
  [m & {:keys [decimal] :or {decimal 10}}]
  (every? #(stochastic-map? % :decimal decimal)
          (vals m)))

(defn log-row-stochastic-map?
  "Returns true if the map is logarithmically stochastic to the given
  precision."
  [m & {:keys [decimal] :or {decimal 10}}]
  (every? #(log-stochastic-map? % :decimal decimal)
          (vals m)))
