(ns hidden-markov-music.math)

(defn sqrt [x] (Math/sqrt x))
(defn pow [x p] (Math/pow x p))

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

(defn mean
  "Returns the mean of a numeric collection."
  [coll]
  (let [sum  (reduce + coll)
        size (count coll)]
    (if (pos? size)
      (/ sum size)
      0)))

(defn variance
  "Returns the variance of a numeric collection. Can optionally provide
  degrees of freedom, `dof`, which defaults to zero."
  ([coll]
     (variance coll 0))
  ([coll dof]
     (let [mean (mean  coll)
           size (count coll)]
       (/ (reduce +
                  (map #(pow (- % mean) 2)
                       coll))
          (- size dof)))))

(defn stdev
  "Returns the standard deviation of a numeric collection. Can optionally
  provide degrees of freedom, `dof`, which defaults to zero."
  ([coll]
     (stdev coll 0))
  ([coll dof]
     (sqrt (variance coll dof))))
