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

(defn- alphas-iter
  "Helper function for computing lazy seq of alphas.

  Computes the next alpha, based on the previous alpha, and returns a lazy
  sequence with the next alpha at the head."
  [model observations alpha-prev]
  ;; return nil when no observations remain
  (when-let [observations (seq observations)]
    (let [;; compute the next α
          alpha-next (alpha model
                            (first observations)
                            alpha-prev)]
      ;; lazily compute the remaining α's
      (cons alpha-next
            (lazy-seq (alphas-iter model
                                   (rest observations)
                                   alpha-next))))))

(defn alphas
  "Returns a lazy seq of α_1(i), α_2(i), ..., α_T(i)."
  [model observations]
  (let [;; compute α_1(i) here because it is special
        alpha-initial (alpha-init model (first observations))]
    ;; construct the lazy seq of α's, with α_1(i) at the head
    (cons alpha-initial
          (lazy-seq (alphas-iter model
                                 (rest observations)
                                 alpha-initial)))))

(defn forward-likelihood
  "Returns P[O|λ], using the forward algorithm.

  This is the likelihood of the observed sequence O given the model λ."
  [model observations]
  (let [;; construct the lazy seq of α's
        as (alphas model observations)
        ;; pull out the final α, α_T(i)
        alpha-final (last as)]
    ;; return the sum over i of α_T(i), which gives P[O|λ]
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

(defn- betas-iter
  "Helper function for computing lazy seq of β's.

  Computes the current β, based on the next β, and returns a lazy sequence with
  the current β at the head."
  [model observations beta-next]
  ;; return nil when no observations remain
  (when-let [observations (seq observations)]
    (let [;; compute the next β
          beta-next (beta model
                          (first observations)
                          beta-next)]
      ;; lazily compute the remaining β's
      (cons beta-next
            (lazy-seq (betas-iter model
                                  (rest observations)
                                  beta-next))))))

(defn betas
  "Returns a lazy seq of β_T(i), β_{T-1}(i), ..., β_1(i)."
  [model observations]
  (let [;; β_T(i) for all states i is 1.0
        beta-final (zipmap (:states model)
                           (repeat 1.0))]
    ;; construct the lazy seq of β's, with β_T(i) at the head
    (cons beta-final
          (lazy-seq (betas-iter model
                                (reverse (rest observations))
                                beta-final)))))

(defn backward-likelihood
  "Returns P[O|λ], using the backward algorithm.

  This is the likelihood of the observed sequence O given the model λ."
  [model observations]
  (let [;; construct the lazy seq of β's
        bs (betas model observations)
        ;; pull out the initial β, β_1(i)
        beta-initial (last bs)
        ;; compute α_1(i)
        alpha-initial (alpha-init model (first observations))]
    ;; P[O|λ] = β_1(1)*α_1(1) + ... + β_1(N)*α_1(N)
    (reduce + (vals (merge-with * beta-initial
                                  alpha-initial)))))

(defn delta-psi-init
  [model obs]
  {:delta
   (zipmap (:states model)
           (for [state (:states model)]
             (* (get-in model [:initial-prob state])
                (get-in model [:observation-prob state obs])))),
   ;; initial state has no preceding states
   :psi nil})

(defn delta-psi
  [model obs delta-prev]
  (let [weighted-deltas
        (zipmap (:states model)
                (for [state (:states model)]
                  (zipmap (:states model)
                          (for [other-state (:states model)]
                            (* (get delta-prev other-state)
                               (get-in model [:transition-prob
                                              other-state state]))))))
        max-entries
        (zipmap (:states model)
                (for [[state entries] weighted-deltas]
                  (apply max-key val entries)))]
    {:delta
     (zipmap (:states model)
             (for [[state [other-state weighted-delta]] max-entries]
               (* weighted-delta
                  (get-in model [:observation-prob other-state obs])))),
     :psi
     (zipmap (:states model)
             (for [[state [other-state weighted-delta]] max-entries]
               other-state))}))

(defn- delta-psis-iter
  [model observations delta-prev]
  (when-let [observations (seq observations)]
    (let [delta-psi-next (delta-psi model
                                    (first observations)
                                    delta-prev)]
      (cons delta-psi-next
            (lazy-seq (delta-psis-iter model
                                       (rest observations)
                                       (:delta delta-psi-next)))))))

(defn delta-psis
  [model observations]
  (let [delta-psi-initial (delta-psi-init model (first observations))]
    (cons delta-psi-initial
          (lazy-seq (delta-psis-iter model
                                     (rest observations)
                                     (:delta delta-psi-initial))))))

(defn- viterbi-backtrack
  [psis state-next]
  (when-let [psi (first psis)]
    (let [state-current (psi state-next)]
      (cons state-current
            (lazy-seq (viterbi-backtrack (rest psis)
                                         state-current))))))

(defn viterbi-path
  [model observations]
  (let [dps (delta-psis model observations)
        deltas (map :delta dps)
        psis   (map :psi   dps)
        delta-final (last deltas)
        [state-final likelihood] (apply max-key val delta-final)
        optimal-state-sequence
        (cons state-final
              (lazy-seq (viterbi-backtrack (reverse psis)
                                           state-final)))]
    {:likelihood     likelihood
     :state-sequence (reverse optimal-state-sequence)}))
