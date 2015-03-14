(ns hidden-markov-music.hmm
  (:require [hidden-markov-music.stats :as stats]))

(defrecord HMM [states
                observations
                transition-prob
                observation-prob
                initial-state-prob])

(defn random-hmm
  "Returns a model with random probabilities."
  [states observations]
  (HMM.
    states
    observations
    (stats/random-row-stochastic-map states states)
    (stats/random-row-stochastic-map states observations)
    (stats/random-stochastic-map states)))

(defn alpha
  "Returns α_t(i), for t > 0."
  [model obs alpha-prev]
  (zipmap (:states model)
          (for [state (:states model)]
            (* (get-in model [:observation-prob state obs])
               (reduce +
                       (for [other-state (:states model)]
                         (* (get-in model [:transition-prob other-state state])
                            (alpha-prev other-state))))))))

(defn alpha-init
  "Returns α_0(i)."
  [model obs]
  (zipmap (:states model)
          (for [state (:states model)]
            (* (get-in model [:initial-state-prob state])
               (get-in model [:observation-prob   state obs])))))

(defn observation-likelihood
  "Returns P(O|λ), the likelihood of the observed sequence given the model."
  [model observations]
  (loop [alpha-prev   (alpha-init model (first observations))
         observations (next observations)]
    (if (seq observations)
      (let [obs        (first observations)
            alpha-next (alpha model obs alpha-prev)]
        (recur alpha-next (next observations)))
      (reduce + (vals alpha-prev)))))


(defn backward-probabilities
  "Returns X, the optimal hidden state sequence given the observed sequence and
  model."
  [model observations])

(defn posterior-marginals
  "Returns the distribution P(X|O), the posterior marginals of all hidden state
  variables, given the observed sequence."
  [observations & more?])

(defn train
  "Trains a model on an observed sequence, using the Baum-Welch algorithm."
  ([model observations]))
