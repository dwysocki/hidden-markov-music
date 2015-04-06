(ns hidden-markov-music.plots
  (:require [incanter.core   :as incant]
            [incanter.charts :as charts]
            [hidden-markov-music.hmm :as hmm]))

(defn training-likelihood
  [model observations iterations]
  (let [model-likelihoods
        (take iterations
              (hmm/train-model-likelihood-seq model observations))
        ;; indices range from [0, iterations)
        i (range iterations)
        ;; pull out the likelihoods from the [model likelihood] seq
        likelihoods (map second model-likelihoods)]
    (doto (charts/scatter-plot i likelihoods
                               :title "Baum-Welch Training"
                               :x-label "Iteration"
                               :y-label "P[O|λ]")
      (incant/save "training-likelihood.png" :width 400 :height 300))))
