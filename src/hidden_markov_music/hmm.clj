(ns hidden-markov-music.hmm
  (:require [hidden-markov-music.stats :as stats]))

(defrecord HMM [possible-states
                possible-observations
                state-transition-probs
                observation-probs
                initial-state-dist])

(defn random-hmm
  "Returns a model with random probabilities."
  [possible-states possible-observations]
  (HMM.
    possible-states
    possible-observations
    (stats/random-row-stochastic-map possible-states
                                     possible-states)
    (stats/random-row-stochastic-map possible-states
                                     possible-observations)
    (stats/random-stochastic-map possible-states)))

(defn- alpha
  "Returns α_{T-1}(i)"
  [model observations state]
  ; ...
  )

(defn forward-probabilities
  "Returns P(O|λ), the likelihood of the observed sequence given the model."
  [model observations]
  (reduce + (map #(alpha model observations %)
                 (:possible-states model))))


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
