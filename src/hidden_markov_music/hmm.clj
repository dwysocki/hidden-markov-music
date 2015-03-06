(ns hidden-markov-music.hmm)

(defstruct hmm
  :state-transition-probs
  :observation-probs
  :initial-state-dist)

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
