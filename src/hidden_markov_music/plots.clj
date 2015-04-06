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

(defn training-likelihood-multiple
  [model multiple-observations iterations repetitions]
  (let [;; count the number of observation sequences
        n-observations
        (count multiple-observations)
        ;; train the models
        [final-model trained-models]
        (reduce (fn [[model tms] obs]
                  (let [new-tms   (take iterations
                                        (rest (hmm/train-model-seq model
                                                                   obs)))
                        new-model (last new-tms)]
                    [new-model (concat tms new-tms)]))
                [model []]
                ;; repeat the observations the given number of times
                (take (* repetitions n-observations)
                      (cycle (vals multiple-observations))))
        ;; generate the iteration indices
        iterations
        (range (* n-observations
                  iterations
                  repetitions))
        ;; compute the likelihoods
        likelihoods
        (into {}
              (for [[name obs] multiple-observations]
                [name (map (fn [model]
                             (hmm/likelihood-forward model obs))
                           trained-models)]))
        plot (doto (charts/xy-plot)
               (charts/set-x-label "Iteration")
               (charts/set-y-label "P[O|λ]")
               (charts/set-title   "Baum-Welch Training Multiple"))]
    (doseq [[name likelihood] likelihoods]
      (charts/add-lines plot
                        iterations likelihood
                        :series-label name))
    plot))
