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

(defn initial-forward-probability
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

(defn next-forward-probability
  "Returns α_t(i), for all states i, for t > 1, where α_t(i) is the probability
  of being in state s_i at time t after observing the sequence
  o_1, o_2, ..., o_t..

  This is the probability of being in state i after observing the observation
  sequence, o_1, ..., o_t. Depends on the model, α_{t-1}(i), and o_t.

  Output is in the format

    {s_1 α_t(1),
     s_2 α_t(2),
     ...
     s_N α_t(N)}"
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

(defn- forward-probabilities-helper
  "Helper function for forward-probabilities.

  Computes the next α, based on the previous α, and returns a lazy sequence
  with the next α at its head."
  [model observations alpha-prev]
  ;; return nil when no observations remain
  (when-let [observations (seq observations)]
    (let [;; compute the next α
          alpha-current (next-forward-probability model
                                                  (first observations)
                                                  alpha-prev)]
      ;; lazily compute the remaining α's
      (cons alpha-current
            (lazy-seq (forward-probabilities-helper model
                                                    (rest observations)
                                                    alpha-current))))))

(defn forward-probabilities
  "Returns a lazy seq of α_1(i), α_2(i), ..., α_T(i), where α_t(i) is the
  probability of being in state s_i at time t after observing the sequence
  o_1, o_2, ..., o_t."
  [model observations]
  (let [;; compute α_1(i) separately because it is special
        alpha-initial (initial-forward-probability model
                                                   (first observations))]
    ;; construct the lazy seq of α's, with α_1(i) at the head
    (cons alpha-initial
          (lazy-seq (forward-probabilities-helper model
                                                  (rest observations)
                                                  alpha-initial)))))

(defn forward-likelihood
  "Returns P[O|λ], using the forward algorithm.

  This is the likelihood of the observed sequence O given the model λ."
  [model observations]
  (let [;; construct the lazy seq of α's
        alphas (forward-probabilities model observations)
        ;; pull out the final α, α_T(i)
        alpha-final (last alphas)]
    ;; return the sum over i of α_T(i), which gives P[O|λ]
    (reduce + (vals alpha-final))))

(defn prev-backward-probability
  "Returns β_t(i), for all states i, for t < T.

  This is the probability of observing the partial observation sequence,
  o_t, o_{t+1}, ..., o_T, conditional on being in state i at time t. Depends on
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

(defn- backward-probabilities-helper
  "Helper function for computing lazy seq of β's.

  Computes the current β, based on the next β, and returns a lazy sequence with
  the current β at its head."
  [model observations beta-next]
  ;; return nil when no observations remain
  (when-let [observations (seq observations)]
    (let [;; compute the next β
          beta-current (prev-backward-probability model
                                                  (first observations)
                                                  beta-next)]
      ;; lazily compute the remaining β's
      (cons beta-current
            (lazy-seq (backward-probabilities-helper model
                                                     (rest observations)
                                                     beta-current))))))

(defn backward-probabilities
  "Returns a lazy seq of β_T(i), β_{T-1}(i), ..., β_1(i)."
  [model observations]
  (let [;; β_T(i) for all states i is 1.0
        beta-final (zipmap (:states model)
                           (repeat 1.0))]
    ;; construct the lazy seq of β's, with β_T(i) at the head
    (cons beta-final
          (lazy-seq (backward-probabilities-helper model
                                                   (reverse (rest observations))
                                                   beta-final)))))

(defn backward-likelihood
  "Returns P[O|λ], using the backward algorithm.

  This is the likelihood of the observed sequence O given the model λ."
  [model observations]
  (let [;; construct the lazy seq of β's
        betas (backward-probabilities model observations)
        ;; pull out the initial β, β_1(i)
        beta-initial (last betas)
        ;; compute α_1(i)
        alpha-initial (initial-forward-probability model
                                                   (first observations))]
    ;; P[O|λ] = β_1(1)*α_1(1) + ... + β_1(N)*α_1(N)
    (reduce + (vals (merge-with * beta-initial
                                  alpha-initial)))))

(defn initial-state-path
  "Returns ψ_1(i) and δ_1(i), for the given model λ and first observation o_1.

  Output takes the form:

    {:delta δ_1(i),
     :psi   ψ_1(i)}"
  [model obs]
  {:delta
   (zipmap (:states model)
           (for [state (:states model)]
             ;; δ_1(i) = π(i)*b_i(o_1)
             (* (get-in model [:initial-prob state])
                (get-in model [:observation-prob state obs])))),
   ;; initial state has no preceding states, so ψ_1(i) = nil
   :psi nil})

(defn next-state-path
  "Returns ψ_t(i) and δ_t(i), for the given model λ and observation o_t.
  Depends on the previous δ_{t-1}(i).

  Output takes the form:

    {:delta δ_t(i),
     :psi   ψ_t(i)}"
  [model obs delta-prev]
  (let [;; this is a mapping of
        ;; state-j -> state-i -> δ_{t-1}(i) p_{ij}
        weighted-deltas
        (zipmap (:states model)
                (for [state (:states model)]
                  (zipmap (:states model)
                          (for [other-state (:states model)]
                            (* (get delta-prev other-state)
                               (get-in model [:transition-prob
                                              other-state state]))))))
        ;; this is a mapping of
        ;; state-j -> [argmax(δ_{t-1}(i) p_{ij}),
        ;;                max(δ_{t-1}(i) p_{ij})]
        max-entries
        (zipmap (keys weighted-deltas)
                (for [[state entries] weighted-deltas]
                  (apply max-key val entries)))]
    {;; this is a mapping of
     ;; state-j -> max(δ_{t-1}(i) p_{ij})*b_j(o_t)
     :delta
     (zipmap (keys max-entries)
             (for [[state [other-state weighted-delta]] max-entries]
               (* weighted-delta
                  (get-in model [:observation-prob state obs])))),
     ;; this is a mapping of
     ;; state-j -> argmax(δ_{t-1}(i) p_{ij})
     :psi
     (zipmap (keys max-entries)
             (for [[state [other-state weighted-delta]] max-entries]
               other-state))}))

(defn- state-paths-helper
  "Helper function for computing lazy seq of ψ's and δ's.

  Computes the current ψ and δ, based on the previous δ, and returns a lazy
  sequence with the current ψ and δ at its head."
  [model observations delta-prev]
  (when-let [observations (seq observations)]
    (let [delta-psi-next (next-state-path model
                                          (first observations)
                                          delta-prev)]
      (cons delta-psi-next
            (lazy-seq (state-paths-helper model
                                          (rest observations)
                                          (:delta delta-psi-next)))))))

(defn state-paths
  "Returns a lazy seq of previous states paired with their probabilities,
  [ψ_1(i) δ_1(i)], ... [ψ_T(i) δ_T(i)],
  where ψ_t(i) is a mapping from state i to the state j which most likely
  preceded it, and
  where δ_t(i) is a mapping from state i to the probability of the most likely
  state path leading up to it from state j."
  [model observations]
  (let [delta-psi-initial (initial-state-path model
                                              (first observations))]
    (cons delta-psi-initial
          (lazy-seq (state-paths-helper model
                                        (rest observations)
                                        (:delta delta-psi-initial))))))

(defn- viterbi-backtrack
  "Lazily constructs the optimal state sequence by backtracking, using

    q_t = ψ_{t+1}(q_{t+1})

  Takes as input ψ_{T-1}(i), ..., ψ_1(i), and q_T."
  [psis state-next]
  (when-let [psi (first psis)]
    (let [state-current (psi state-next)]
      (cons state-current
            (lazy-seq (viterbi-backtrack (rest psis)
                                         state-current))))))

(defn viterbi-path
  "Returns one of the state sequences Q which maximizes P[Q|O,λ], along with
  the likelihood itself, P[Q|O,λ]. There are potentially many such paths, all
  with equal likelihood, and one of those is chosen randomly.

  This is accomplished by means of the Viterbi algorithm, and takes into
  account that q_t depends on q_{t-1}, and not just o_t.

  Output takes the form:

    {:likelihood     P[Q|O,λ],
     :state-sequence Q}"
  [model observations]
  (let [;; compute a lazy seq of [ψ_1(i) δ_1(i)], ... [ψ_T(i) δ_T(i)]
        delta-psis (state-paths model observations)
        ;; pull the δ's and ψ's from this lazy seq
        deltas     (map :delta delta-psis)
        psis       (map :psi   delta-psis)
        ;; the only δ we need is δ_T(i)
        delta-final (last deltas)
        ;; the final state is the state i which maximizes δ_T(i),
        ;; and the associated value is the likelihood of the associated
        ;; state sequence
        [state-final likelihood] (apply max-key val delta-final)
        ;; construct the optimal state sequence by starting with the final
        ;; state and backtracking over the ψ's
        optimal-state-sequence
        (cons state-final
              (lazy-seq (viterbi-backtrack (reverse psis)
                                           state-final)))]
    {:likelihood     likelihood
     ;; state sequence was constructed via backtracking, and must be reversed
     :state-sequence (reverse optimal-state-sequence)}))
