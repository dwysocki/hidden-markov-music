(ns hidden-markov-music.hmm
  "General implementation of a hidden Markov model, and associated algorithms."
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
  sequence, O_1, ..., O_t. Depends on the model, α_{t-1}(i), and the most
  recent observation.

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

(defn forward-likelihood
  "Returns P[O|λ], using the forward algorithm.

  This is the likelihood of the observed sequence O given the model λ."
  [model observations]
  (let [;; α_1(i) for all states i
        alpha-initial (alpha-init model (first observations))
        ;; α_T(i) for all states i
        alpha-final   (reduce
                        ;; α_t(i) depends on α_{t-1}(j) and O_t
                        (fn [alpha-prev obs]
                          (alpha model obs alpha-prev))
                        ;; provide basis to recursion
                        alpha-initial
                        ;; O_1 already included in alpha-initial,
                        ;; so operate only on O_2, ..., O_T
                        (next observations))]
    ;; return the sum of α_T(i) for all states i
    (reduce + (vals alpha-final))))

(defn beta
  "Returns β_t(i), for all states i, for t < T.

  This is the probability of observing the partial observation sequence,
  O_t, O_{t+1}, ..., O_T, conditional on being in state i at time t. Depends on
  the model, β_{t+1}(j), and the most recent observation.

  Output is in the format

    {:state-1 β_t(1),
     :state-2 β_t(2),
     ...
     :state-N β_t(N)}"
  [model obs beta-next]
  ;; map each state to its β
  (zipmap (:states model)
          (for [state (:states model)]
            ;; compute β_t for the given state
            (reduce +
                    (for [other-state (:states model)]
                      (* (get-in model
                                 [:transition-prob state other-state])
                         (beta-next other-state)
                         (get-in model
                                 [:observation-prob other-state obs])))))))

(defn backward-likelihood
  "Returns P[O|λ], using the backward algorithm.

  This is the likelihood of the observed sequence O given the model λ."
  [model observations]
  (let [;; β_T(i) for all states i is 1.0
        beta-final   (zipmap (:states model)
                             (repeat 1.0))
        ;; β_1(i) for all states i
        beta-initial (reduce
                       ;; β_t(i) depends on β_{t+1}(j) and O_t
                       (fn [beta-next obs]
                         (beta model obs beta-next))
                       ;; provide basis to recursion
                       beta-final
                       ;; O_1 will be included in the final reduction,
                       ;; so operate only on O_T, O_{T-1}, ..., O_3, O_2
                       (reverse (next observations)))
        ;; α_1(i) is needed to compute P[O|λ]
        alpha-initial (alpha-init model (first observations))]
    ;; P[O|λ] = β_1(1)*α_1(1) + ... + β_1(N)*α_1(N)
    (reduce + (vals (merge-with * beta-initial alpha-initial)))))


(defn posterior-marginals
  "Returns the distribution P(X|O), the posterior marginals of all hidden state
  variables, given the observed sequence."
  [observations & more?])

(defn train
  "Trains a model on an observed sequence, using the Baum-Welch algorithm."
  ([model observations]))
