(ns hidden-markov-music.math)

(defn exp [x] (Math/exp x))
(defn log [x] (Math/log x))

(defn log-sum [log-x log-y]
  (cond
    (= log-x Double/NEGATIVE_INFINITY) log-y
    (= log-y Double/NEGATIVE_INFINITY) log-x
    :else (let [[min max] (sort [log-x log-y])]
            (+ max (log (inc (exp (- min max))))))))

(defn log-product [log-x log-y]
  (if (some (partial = Double/NEGATIVE_INFINITY)
            [log-x log-y])
    Double/NEGATIVE_INFINITY
    (+ log-x log-y)))
