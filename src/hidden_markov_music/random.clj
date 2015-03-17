(ns hidden-markov-music.random)

(defn rand*
  "Returns a random floating point number between 0 (exclusive) and
  n (default 1) (inclusive)."
  ([]
     (rand* 1.0))
  ([n]
     (- n (rand n))))

(defn select-random-key
  "Takes a collection of `[key prob]` pairs, whose `prob`s sum to 1.0, and
  selects a `key` randomly based on its `prob`."
  [key->prob]
  (loop [key->prob       key->prob
         selection-index (rand*)]
    (let [[k p] (first key->prob)
          new-selection-index (- selection-index p)]
      (if (pos? new-selection-index)
        (recur (rest key->prob) new-selection-index)
        k))))
