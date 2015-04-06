(ns hidden-markov-music.hmm
  "General implementation of a hidden Markov model, and associated algorithms."
  (:require [hidden-markov-music.math :refer [exp log log-sum log-product]]
            [hidden-markov-music.stats :as  stats]
            [hidden-markov-music.random :refer [select-random-key]]
            [hidden-markov-music.util :refer [map-for map-vals
                                              numbers-almost-equal?
                                              maps-almost-equal?]])
  (:use clojure.pprint))

(defn- model-class [model & args]
  (class model))

(defrecord HMM
    [states
     observations
     initial-prob
     transition-prob
     observation-prob])

(defrecord LogHMM
    [states
     observations
     initial-prob
     transition-prob
     observation-prob])


;; HMM and LogHMM have no docstrings, but the functions they generate do.
;; Attach some more descriptive text to those functions' docstrings
(doseq [f [#'->HMM #'map->HMM]]
  (alter-meta! f update-in [:doc] str
    "\nConstructs a representation of a hidden Markov model."))
(doseq [f [#'->LogHMM #'map->LogHMM]]
  (alter-meta! f update-in [:doc] str
    "\nConstructs a logarithmic representation of a hidden Markov model."))

(defn HMM->LogHMM
  "Transforms an HMM into a logarithmic HMM."
  [model]
  (LogHMM.
    (:states model)
    (:observations model)
    (map-vals log (:initial-prob model))
    (map-vals (partial map-vals log) (:transition-prob model))
    (map-vals (partial map-vals log) (:observation-prob model))))

(defn LogHMM->HMM
  "Transforms a logarithmic HMM into an HMM."
  [model]
  (HMM.
    (:states model)
    (:observations model)
    (map-vals exp (:initial-prob model))
    (map-vals (partial map-vals exp) (:transition-prob model))
    (map-vals (partial map-vals exp) (:observation-prob model))))

(defmulti hmms-almost-equal?
  "Returns true if two HMMs are equal to the given precision."
  (fn [x y & {:keys [decimal] :or {decimal 6}}]
    [(class x) (class y)]))

(defn- hmms-almost-equal-helper
  [x y decimal]
  (and (maps-almost-equal? (:initial-prob x)
                           (:initial-prob y)
                           :decimal decimal)
       (maps-almost-equal? (:transition-prob x)
                           (:transition-prob y)
                           :decimal decimal)
       (maps-almost-equal? (:observation-prob x)
                           (:observation-prob y)
                           :decimal decimal)))

(defmethod hmms-almost-equal? [HMM HMM]
  [x y & {:keys [decimal] :or {decimal 6}}]
  (hmms-almost-equal-helper x y decimal))

(defmethod hmms-almost-equal? [LogHMM LogHMM]
  [x y & {:keys [decimal] :or {decimal 6}}]
  (hmms-almost-equal-helper x y decimal))

(defmethod hmms-almost-equal? :default
  [x y & args]
  false)

(defmulti valid-hmm?
  "Returns true if the HMM has all stochastic probabilities to the given
  precision."
  model-class)

(defmethod valid-hmm? HMM
  [model & {:keys [decimal] :or {decimal 10}}]
  (and (stats/stochastic-map?     (:initial-prob     model)
                                  :decimal decimal)
       (stats/row-stochastic-map? (:transition-prob  model)
                                  :decimal decimal)
       (stats/row-stochastic-map? (:observation-prob model)
                                  :decimal decimal)))

(defmethod valid-hmm? LogHMM
  [model & {:keys [decimal] :or {decimal 10}}]
  (and (stats/log-stochastic-map?     (:initial-prob     model)
                                      :decimal decimal)
       (stats/log-row-stochastic-map? (:transition-prob  model)
                                      :decimal decimal)
       (stats/log-row-stochastic-map? (:observation-prob model)
                                      :decimal decimal)))

(defn random-HMM
  "Returns an HMM with random probabilities, given the state and observation
  labels."
  [states observations]
  (HMM.
    states
    observations
    (stats/random-stochastic-map states)
    (stats/random-row-stochastic-map states states)
    (stats/random-row-stochastic-map states observations)))

(defn random-LogHMM
  "Returns a logarithmic HMM with random probabilities, given the state and
  observation labels."
  [states observations]
  (HMM->LogHMM (random-HMM states observations)))


(defmulti forward-probability-initial
  "Returns `α_1(i)`, for all states `i`.

  This is the probability of initially being in state `i` after observing the
  initial observation, `o_1`. Depends only on the model and initial observation.

  Output is in the format

  ```
  {:state-1 α_1(1),
   :state-2 α_1(2),
   ...
   :state-N α_1(N)}
  ```"
  model-class)

(defmethod forward-probability-initial HMM
  [model obs]
  ;; map each state to its initial α
  (map-for [state (:states model)]
           ;; compute α_1 for the given state
           (* (get-in model [:initial-prob     state])
              (get-in model [:observation-prob state obs]))))

(defmethod forward-probability-initial LogHMM
  [model obs]
  ;; map each state to its initial log α
  (map-for [state (:states model)]
           ;; compute α_1 for the given state
           (+ (get-in model [:initial-prob     state])
              (get-in model [:observation-prob state obs]))))

(defmulti forward-probability-next
  "Returns `α_t(i)`, for all states `i`, for `t > 1`, where `α_t(i)` is the
  probability of being in state `s_i` at time `t` after observing the sequence
  `o_1, o_2, ..., o_t`. Depends on the model `λ`, previous forward probability
  `α_{t-1}(i)`, and current observation `o_t`.

  Output is in the format

  ```
  {s_1 α_t(1),
   s_2 α_t(2),
   ...
   s_N α_t(N)}
  ```"
  model-class)

(defmethod forward-probability-next HMM
  [model obs alpha-prev]
  ;; map each state to its α
  (map-for [state (:states model)]
           ;; compute α_t for the given state
           (* (get-in model [:observation-prob state obs])
              (reduce +
                      (for [other-state (:states model)]
                        (* (get-in model [:transition-prob other-state state])
                           (alpha-prev other-state)))))))

(defmethod forward-probability-next LogHMM
  [model obs log-alpha-prev]
  ;; map each state to its α
  (map-for [state-j (:states model)]
           ;; compute α_t for the given state
           (let [log-alpha
                 (reduce (fn [log-alpha state-i]
                           (log-sum log-alpha
                                    (+ (get-in log-alpha-prev [state-i])
                                       (get-in model [:transition-prob
                                                      state-i
                                                      state-j]))))
                         Double/NEGATIVE_INFINITY
                         (:states model))]
             (+ log-alpha
                (get-in model [:observation-prob state-j obs])))))

(defn- forward-probability-helper
  "Helper function for computing lazy seq of `α`'s.

  Computes the current `α`, based on the previous `α`, and returns a lazy
  sequence with the current `α` at its head."
  [model observations alpha-prev]
  ;; return nil when no observations remain
  (when-let [observations (seq observations)]
    (let [;; compute the next α
          alpha-current (forward-probability-next model
                                                  (first observations)
                                                  alpha-prev)]
      ;; lazily compute the remaining α's
      (cons alpha-current
            (lazy-seq (forward-probability-helper model
                                                  (rest observations)
                                                  alpha-current))))))

(defn forward-probability-seq
  "Returns a lazy seq of `α_1(i), α_2(i), ..., α_T(i)`, where `α_t(i)` is the
  probability of being in state `s_i` at time `t` after observing the sequence
  `o_1, o_2, ..., o_t`."
  [model observations]
  (let [;; compute α_1(i) separately because it is special
        alpha-initial (forward-probability-initial model
                                                   (first observations))]
    ;; construct the lazy seq of α's, with α_1(i) at the head
    (cons alpha-initial
          (lazy-seq (forward-probability-helper model
                                                (rest observations)
                                                alpha-initial)))))

(defmulti likelihood-forward
  "Returns `P[O|λ]`, using the forward algorithm.

  This is the likelihood of the observed sequence `O` given the model `λ`."
  model-class)

(defmethod likelihood-forward HMM
  [model observations]
  (let [;; construct the lazy seq of α's
        alphas (forward-probability-seq model observations)
        ;; pull out the final α, α_T(i)
        alpha-final (last alphas)]
    ;; return the sum over i of α_T(i), which gives P[O|λ]
    (reduce + (vals alpha-final))))

(defmethod likelihood-forward LogHMM
  [model observations]
  (let [;; construct the lazy seq of α's
        alphas (forward-probability-seq model observations)
        ;; pull out the final α, α_T(i)
        alpha-final (last alphas)]
    ;; return the sum over i of α_T(i), which gives P[O|λ]
    (reduce log-sum (vals alpha-final))))


(defmulti backward-probability-final
  "Returns `β_T(i)`, for all states `i`."
  model-class)

(defmethod backward-probability-final HMM
  [model]
  (zipmap (:states model)
          (repeat 1.0)))

(defmethod backward-probability-final LogHMM
  [model]
  (zipmap (:states model)
          (repeat 0.0)))

(defmulti backward-probability-prev
  "Returns `β_t(i)`, for all states `i`, for `t < T`.

  This is the probability of observing the partial observation sequence,
  `o_{t+1}, ..., o_T`, conditional on being in state `i` at time `t`.
  Depends on the model, `β_{t+1}(j)`, and the next observation `o_{t+1}`.

  Output is in the format

  ```
  {:state-1 β_t(1),
   :state-2 β_t(2),
   ...
   :state-N β_t(N)}
  ```"
  model-class)


(defmethod backward-probability-prev HMM
  [model obs beta-next]
  ;; map each state to its β
  (map-for [state (:states model)]
           ;; compute β_t for the given state
           (reduce +
                   (for [other-state (:states model)]
                     (* (get-in model
                                [:transition-prob state other-state])
                        (beta-next other-state)
                        (get-in model
                                [:observation-prob other-state obs]))))))

(defmethod backward-probability-prev LogHMM
  [model obs log-beta-next]
  ;; map each state to its β
  (map-for [state-i (:states model)]
           ;; compute β_t for the given state
           (reduce (fn [log-beta state-j]
                     (log-sum log-beta
                              (+ (get-in model [:transition-prob
                                                state-i state-j])
                                 (get-in model [:observation-prob
                                                state-j obs])
                                 (get-in log-beta-next
                                         [state-j]))))
                   Double/NEGATIVE_INFINITY
                   (:states model))))


(defn- backward-probability-helper
  "Helper function for computing lazy seq of `β`'s.

  Computes the current `β`, based on the next `β`, and returns a lazy sequence
  with the current `β` at its head."
  [model observations beta-next]
  ;; return nil when no observations remain
  (when-let [observations (seq observations)]
    (let [;; compute the next β
          beta-current (backward-probability-prev model
                                                  (first observations)
                                                  beta-next)]
      ;; lazily compute the remaining β's
      (cons beta-current
            (lazy-seq (backward-probability-helper model
                                                   (rest observations)
                                                   beta-current))))))

(defn backward-probability-seq
  "Returns a lazy seq of `β_T(i), β_{T-1}(i), ..., β_1(i)`, where `β_t(i)` is
  the probability of observing `o_{t+1}, ..., o_T`, given that the system is in
  state `s_i` at time `t`."
  [model observations]
  (let [;; β_T(i) for all states i is 1.0
        beta-final (backward-probability-final model)]
    ;; construct the lazy seq of β's, with β_T(i) at the head
    (cons beta-final
          (lazy-seq (backward-probability-helper model
                                                 (reverse (rest observations))
                                                 beta-final)))))

(defmulti likelihood-backward
  "Returns `P[O|λ]`, using the backward algorithm.

  This is the likelihood of the observed sequence `O` given the model `λ`."
  model-class)

(defmethod likelihood-backward HMM
  [model observations]
  (let [;; construct the lazy seq of β's
        betas (backward-probability-seq model observations)
        ;; pull out the initial β, β_1(i)
        beta-initial (last betas)
        ;; compute α_1(i)
        alpha-initial (forward-probability-initial model
                                                   (first observations))]
    ;; P[O|λ] = β_1(1)*α_1(1) + ... + β_1(N)*α_1(N)
    (reduce + (vals (merge-with * beta-initial
                                  alpha-initial)))))

(defmethod likelihood-backward LogHMM
  [model observations]
  (let [;; construct the lazy seq of β's
        betas (backward-probability-seq model observations)
        ;; pull out the initial β, β_1(i)
        beta-initial (last betas)
        ;; compute α_1(i)
        alpha-initial (forward-probability-initial model
                                                   (first observations))]
    ;; P[O|λ] = β_1(1)*α_1(1) + ... + β_1(N)*α_1(N)
    (reduce log-sum (vals (merge-with log-product beta-initial
                                                  alpha-initial)))))

(defmulti state-path-initial
  "Returns `ψ_1(i)` and `δ_1(i)`, for the given model `λ` and first observation
  `o_1`.

  Output takes the form:

  ```
  {:delta δ_1(i),
   :psi   ψ_1(i)}
  ```"
  model-class)

(defmethod state-path-initial HMM
  [model obs]
  {:delta
   (map-for [state (:states model)]
            ;; δ_1(i) = π(i)*b_i(o_1)
            (* (get-in model [:initial-prob state])
               (get-in model [:observation-prob state obs]))),
   ;; initial state has no preceding states, so ψ_1(i) = nil
   :psi nil})

(defmethod state-path-initial LogHMM
  [model obs]
  {:delta
   (map-for [state (:states model)]
            ;; δ_1(i) = π(i)*b_i(o_1)
            (+ (get-in model [:initial-prob state])
               (get-in model [:observation-prob state obs]))),
   ;; initial state has no preceding states, so ψ_1(i) = nil
   :psi nil})

(defmulti weighted-deltas
  "Returns a mapping of `state-j -> state-i -> δ_{t-1}(i) p_{ij}`."
  model-class)

(defmethod weighted-deltas HMM
  [model delta-prev]
  (map-for [state-i (:states model)
            state-j (:states model)]
           (* (get delta-prev state-j)
              (get-in model [:transition-prob
                             state-j state-i]))))

(defmethod weighted-deltas LogHMM
  [model log-delta-prev]
  (map-for [state-i (:states model)
            state-j (:states model)]
           (+ (get log-delta-prev state-j)
              (get-in model [:transition-prob
                             state-j state-i]))))

(defmulti deltas
  "Returns a mapping of `state-j -> max(δ_{t-1}(i) p_{ij})*b_j(o_t)`."
  model-class)

(defmethod deltas HMM
  [model max-entries obs]
  (zipmap (keys max-entries)
          (for [[state [other-state weighted-delta]] max-entries]
            (* weighted-delta
               (get-in model [:observation-prob state obs])))))

(defmethod deltas LogHMM
  [model max-entries obs]
  (zipmap (keys max-entries)
          (for [[state [other-state weighted-delta]] max-entries]
            (+ weighted-delta
               (get-in model [:observation-prob state obs])))))

(defn psis
  "Returns a mapping of `state-j -> argmax(δ_{t-1}(i) p_{ij})`."
  [max-entries]
  (zipmap (keys max-entries)
          (for [[state [other-state weighted-delta]] max-entries]
            other-state)))

(defn max-entries
  "Returns a mapping of
  ```
  state-j -> [argmax(δ_{t-1}(i) p_{ij}),
                 max(δ_{t-1}(i) p_{ij})]
  ```"
  [weighted-deltas]
  (zipmap (keys weighted-deltas)
          (for [[state entries] weighted-deltas]
            (apply max-key val entries))))

(defn state-path-next
  "Returns `ψ_t(i)` and `δ_t(i)`, for the given model `λ` and current
  observation `o_t`. Depends on the previous `δ_{t-1}(i)`.

  Output takes the form:

  ```
  {:delta δ_t(i),
   :psi   ψ_t(i)}
  ```"
  [model obs delta-prev]
  (let [weighted-deltas (weighted-deltas model delta-prev)
        max-entries     (max-entries weighted-deltas)]
    {:delta (deltas model max-entries obs),
     :psi   (psis max-entries)}))

(defn- state-path-helper
  "Helper function for computing lazy seq of `ψ`'s and `δ`'s.

  Computes the current `ψ` and `δ`, based on the previous `δ`, and returns a
  lazy sequence with the current `ψ` and `δ` at its head."
  [model observations delta-prev]
  (when-let [observations (seq observations)]
    (let [delta-psi-next (state-path-next model
                                          (first observations)
                                          delta-prev)]
      (cons delta-psi-next
            (lazy-seq (state-path-helper model
                                         (rest observations)
                                         (:delta delta-psi-next)))))))

(defn state-path-seq
  "Returns a lazy seq of previous states paired with their probabilities,
  `[ψ_1(i) δ_1(i)], ... [ψ_T(i) δ_T(i)]`,
  where `ψ_t(i)` is a mapping from state `i` to the state `j` which most likely
  preceded it, and `δ_t(i)` is a mapping from state `i` to the probability of
  the most likely state path leading up to it from state `j`."
  [model observations]
  (let [delta-psi-initial (state-path-initial model
                                              (first observations))]
    (cons delta-psi-initial
          (lazy-seq (state-path-helper model
                                       (rest observations)
                                       (:delta delta-psi-initial))))))

(defn- viterbi-backtrack
  "Lazily constructs the optimal state sequence by backtracking, using

  ```
  q_t = ψ_{t+1}(q_{t+1})
  ```

  Takes as input `ψ_{T-1}(i), ..., ψ_1(i)`, and `q_T`."
  [psis state-next]
  (when-let [psi (first psis)]
    (let [state-current (psi state-next)]
      (cons state-current
            (lazy-seq (viterbi-backtrack (rest psis)
                                         state-current))))))

(defn viterbi-path
  "Returns one of the state sequences `Q` which maximizes `P[Q|O,λ]`, along
  with the likelihood itself, `P[Q|O,λ]`. There are potentially many such
  paths, all with equal likelihood, and one of those is chosen arbitrarily.

  This is accomplished by means of the Viterbi algorithm, and takes into
  account that `q_t` depends on `q_{t-1}`, and not just `o_t`, avoiding
  impossible state sequences.

  Output takes the form:

  ```
  {:likelihood     P[Q|O,λ],
   :state-sequence Q}
  ```"
  [model observations]
  (let [;; compute a lazy seq of [ψ_1(i) δ_1(i)], ... [ψ_T(i) δ_T(i)]
        delta-psis (state-path-seq model observations)
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






(defmulti gamma
  "Returns the probability of being in state `i` at time `t` given the model
  and observation sequence."
  model-class)

(defmethod gamma HMM
  [model forward-prob backward-prob]
  (map-for [state (:states model)]
           (/ (* (forward-prob  state)
                 (backward-prob state))
              (reduce +
                      (for [other-state (:states model)]
                        (* (backward-prob other-state)
                           (forward-prob  other-state)))))))

(defmethod gamma LogHMM
  [model log-forward-prob log-backward-prob]
  (let [[normalizer partial-log-gammas]
        (reduce (fn [[normalizer partial-log-gammas] state]
                  (let [partial-log-gamma (+ (get log-forward-prob  state)
                                             (get log-backward-prob state))
                        normalizer        (log-sum normalizer
                                                   partial-log-gamma)]
                    [normalizer (assoc partial-log-gammas
                                  state partial-log-gamma)]))
                [Double/NEGATIVE_INFINITY {}]
                (:states model))]
    (map-for [state (:states model)]
             (- (get partial-log-gammas state)
                normalizer))))

(defn gamma-seq
  "Returns a lazy sequence of gammas from `t = 1` to `t = T`.

  See [[gamma]]."
  [model forward-probs backward-probs]
  (map (partial gamma model)
       forward-probs
       backward-probs))

(defmulti digamma
  "Returns the probability of being in state `i` at time `t` and state `j` at
  time `t+1` given the model and observation sequence."
  model-class)

(defmethod digamma HMM
  [model forward-prob backward-prob-next observation-next]
  (let [likelihood
        (->> (for [state-i (:states model)
                   state-j (:states model)]
               (* (forward-prob state-i)
                  (get-in model [:transition-prob
                                 state-i
                                 state-j])
                  (get-in model [:observation-prob
                                 state-j
                                 observation-next])
                  (backward-prob-next state-j)))
             flatten
             (reduce +))]
    (map-for [state-current (:states model)
              state-next    (:states model)]
             (/ (* (forward-prob state-current)
                   (get-in model [:transition-prob
                                  state-current
                                  state-next])
                   (get-in model [:observation-prob
                                  state-next
                                  observation-next])
                   (backward-prob-next state-next))
                likelihood))))

(defmethod digamma LogHMM
  [model log-forward-prob log-backward-prob-next obs-next]
  (let [partial-log-digammas
        (map-for [state-i (:states model)
                  state-j (:states model)]
                 (+ (get log-forward-prob state-i)
                    (get log-backward-prob-next state-j[])
                    (get-in model [:transition-prob state-i state-j])
                    (get-in model [:observation-prob state-j obs-next])))

        normalizer
        (reduce log-sum
                Double/NEGATIVE_INFINITY
                (flatten (map vals (vals partial-log-digammas))))]
    (map-for [state-i (:states model)
              state-j (:states model)]
             (- (get-in partial-log-digammas [state-i state-j])
                normalizer))))

(defn digamma-seq
  "Returns a lazy sequence of digammas from `t = 1` to `t = T-1`.

  See [[digamma]]."
  [model forward-probs backward-probs observations]
  (map (partial digamma model)
       forward-probs
       (rest backward-probs)
       (rest observations)))

(defn- train-initial-probs
  [gammas]
  (first gammas))

(defmulti ^:private train-transition-probs
  "Returns an updated transition probability matrix given the gammas and
  digammas computed for the model."
  model-class)

(defmethod train-transition-probs HMM
  [model gammas digammas]
  (map-for [state-current (:states model)]
    (let [expected-transitions
          (->> gammas
               butlast
               (map #(get % state-current))
               (reduce +))]
      (map-for [state-next (:states model)]
        (/ (->> digammas
                (map #(get-in % [state-current state-next]))
                (reduce +))
           expected-transitions)))))

(defmethod train-transition-probs LogHMM
  [model gammas digammas]
  (map-for [state-i (:states model)
            state-j (:states model)]
    (let [[numerator denominator]
          (reduce (fn [[num denom] [gamma digamma]]
                    [(log-sum num   (get-in digamma [state-i state-j])),
                     (log-sum denom (get gamma state-i))])
                  [Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY]
                  (map vector
                       gammas digammas))]
      (- numerator denominator))))

(defmulti ^:private train-observation-probs
  "Returns an updated observation probability matrix given the gammas computed
  for the model, and the observation sequence."
  model-class)

(defmethod train-observation-probs HMM
  [model gammas observations]
  (map-for [state-current (:states model)]
           (let [expected-transitions (->> gammas
                                           (map #(get % state-current))
                                           (reduce +))]
             (map-for [obs (:observations model)]
                      (->> (map vector gammas observations)
                           (filter (fn [[g o]] (= o obs)))
                           (map (fn [[g o]] (g state-current)))
                           (reduce +))))))

(defmethod train-observation-probs LogHMM
  [model gammas observations]
  (map-for [state (:states       model)
            obs   (:observations model)]
    (let [[numerator denominator]
          (reduce (fn [[num denom] [gamma o]]
                    [;; update numerator if observation at time t is obs
                     (if (= obs o)
                       (log-sum num (get gamma state))
                       num)
                     ;; update denominator
                     (log-sum denom (get gamma state))])
                  [Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY]
                  (map vector
                       gammas
                       observations))]
      (- numerator denominator))))

(defn train-model-seq
  [model observations]
  (let [alphas   (forward-probability-seq  model observations)
        betas    (reverse (backward-probability-seq model observations))
        gammas   (gamma-seq model alphas betas)
        digammas (digamma-seq model alphas betas observations)

        new-initial-probs     (train-initial-probs gammas)
        new-transition-probs  (train-transition-probs  model gammas digammas)
        new-observation-probs (train-observation-probs model gammas
                                                       observations)
        new-model (assoc model
                    :initial-prob     new-initial-probs
                    :transition-prob  new-transition-probs
                    :observation-prob new-observation-probs)]
    (cons new-model
          (lazy-seq (train-model-seq new-model observations)))))

(defn train-model
  "Trains the model via the Baum-Welch algorithm."
  ([model observations & {:keys [decimal    max-iter]
                          :or   {decimal 15 max-iter 100}}]
     (let [;; generate the infinite lazy seq of trained models,
           ;; and take the maximum number of them
           trained-models
           (take max-iter
                 (cons model
                       (train-model-seq model
                                        observations)))
           ;; associate with each trained model its likelihood
           trained-model-likelihoods
           (map (fn [model]
                  [model (likelihood-forward model observations)])
                trained-models)
           ;; create a sliding window of pairs of trained-models
           trained-model-likelihood-pairs
           (partition 2 1 trained-model-likelihoods)]
       (->> trained-model-likelihood-pairs
            ;; take from the list until convergence to the given decimal place
            ;; is reached
            (take-while (fn [[[model-prev likelihood-prev]
                              [model      likelihood     ]]]
                          (not (numbers-almost-equal? likelihood-prev
                                                      likelihood
                                                      :decimal decimal))))
            ;; the last element is where convergence happened
            last
            ;; extract the [model likelihood] pair later in the window
            second
            ;; extract the model itself from the [model likelihood] pair
            first))))



(defn random-initial-state
  "Randomly selects an initial state from the model, weighed by the initial
  probability distribution."
  [model]
  (select-random-key (:initial-prob model)))

(defn random-transition
  "Randomly selects a state to transition to from the current state, weighed
  by the transition probability distribution."
  [model state]
  (-> model
      (get-in [:transition-prob state])
      select-random-key))

(defn random-emission
  "Randomly emits an observation from the current state, weighed by the
  observation probability distribution."
  [model state]
  (-> model
      (get-in [:observation-prob state])
      select-random-key))

(defmulti sample-states
  "Randomly walks through the states of the model, returning an infinite lazy
  seq of those states.

  See [[random-initial-state]] and [[random-transition]] for details on the
  decisions made at each step."
  model-class)

(defmethod sample-states HMM
  [model]
  (iterate (partial random-transition model)
           (random-initial-state model)))

(defmethod sample-states LogHMM
  [model]
  (sample-states (LogHMM->HMM model)))

(defmulti sample-emissions
  "Randomly walks through the states of the model, returning an infinite lazy
  seq of emissions from those states. One can optionally provide predetermined
  states, and emissions will be made from it randomly.

  See [[sample-states]] and [[random-emission]] for details."
  model-class)

(defmethod sample-emissions HMM
  ([model]
     (sample-emissions model (sample-states model)))
  ([model states]
     (map (partial random-emission model)
          states)))

(defmethod sample-emissions LogHMM
  ([model]
     (sample-emissions (LogHMM->HMM model)))
  ([model states]
     (sample-emissions (LogHMM->HMM model)
                       states)))
