(ns hidden-markov-music.hmm
  (:require [hidden-markov-music.stats :as stats]))

(defrecord HMM [state-transition-probs
                observation-probs
                initial-state-dist])

(defn- random-state-transition-probs
  "Returns a randomly generated transition probability map."
  [possible-states]
  (let [n-states (count possible-states)]
    (into {}
          (for [state possible-states]
            (let [row-probs (stats/random-stochastic-vector n-states)]
              [state
               (into {}
                     (map (fn [s p] [s p])
                          possible-states
                          row-probs))])))))

(defn random-hmm
  "Returns a model with random probabilities."
  [possible-states possible-observations]
  (HMM.
   (stats/random-row-stochastic-map possible-states
                                    possible-states)
   (stats/random-row-stochastic-map possible-states
                                    possible-observations)
   (stats/random-stochastic-map possible-states)))

(defn forward-probabilities
  "Returns P(O|λ), the likelihood of the observed sequence given the model."
  [observations model])

(defn backward-probabilities
  "Returns X, the optimal hidden state sequence given the observed sequence and
  model."
  [observations model])

(defn posterior-marginals
  "Returns the distribution P(X|O), the posterior marginals of all hidden state
  variables, given the observed sequence."
  [observations & more?])

(defn train
  "Trains a model on an observed sequence, using the Baum-Welch algorithm."
  ([observations model]))
