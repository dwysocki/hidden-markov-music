(ns hidden-markov-music.hmm
  "General implementation of a hidden Markov model."
  (:require [hidden-markov-music.stats :as stats]))

(defrecord HMM
    [states
     observations
     transition-prob
     observation-prob
     initial-prob])

(defn random-hmm
  "Returns a model with random probabilities, given the state and observation
  labels."
  [states observations]
  (HMM.
    states
    observations
    (stats/random-row-stochastic-map states states)
    (stats/random-row-stochastic-map states observations)
    (stats/random-stochastic-map states)))

(defn alpha-init
  "Returns α_1(i), for all states i.

  This is the probability of initially being in state i after observing the
  initial observation, O_1. Depends only on the model and initial observation.

  Output is in the format

    {:state-1 α_1(1),
     :state-2 α_1(2),
     ...
     :state-N α_1(N)}"
  [model obs]
  ;; map each state to its initial α
  (zipmap (:states model)
          (for [state (:states model)]
            ;; compute α_1 for the given state
            (* (get-in model [:initial-prob     state])
               (get-in model [:observation-prob state obs])))))

(defn alpha
  "Returns α_t(i), for all states i, for t > 1.

  This is the probability of being in state i after observing the observation
  sequence, O_1, ..., O_t. Depends on the model, α_{t-1}(i), and most recent
  observation.

  Output is in the format

    {:state-1 α_t(1),
     :state-2 α_t(2),
     ...
     :state-N α_t(N)}"
  [model obs alpha-prev]
  ;; map each state to its α
  (zipmap (:states model)
          (for [state (:states model)]
            ;; compute α_t for the given state
            (* (get-in model [:observation-prob state obs])
               (reduce +
                       (for [other-state (:states model)]
                         (* (get-in model [:transition-prob other-state state])
                            (alpha-prev other-state))))))))

(defn observation-likelihood
  "Returns P(O|λ).

  This is the likelihood of the observed sequence O given the model λ. Makes
  use of the forward algorithm."
  [model observations]
  (let [;; α_1(i) for all states i
        alpha-initial (alpha-init model (first observations))
        ;; α_T(i) for all states i
        alpha-final   (reduce
                        ;; α_t(i) depends on α_{t-1}(i) and O_i
                        (fn [alpha-prev obs]
                          (alpha model obs alpha-prev))
                        ;; provide basis to recursion
                        alpha-initial
                        ;; O_1 already included in alpha-initial,
                        ;; so operate only on O_2, ..., O_T
                        (next observations))]
    ;; return the sum of α_T(i) for all states i
    (reduce + (vals alpha-final))))


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
