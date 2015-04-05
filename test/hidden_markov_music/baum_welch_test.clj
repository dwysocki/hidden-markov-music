(ns hidden-markov-music.baum-welch-test
  (:require [hidden-markov-music.hmm         :as hmm]
            [hidden-markov-music.test-models :as tm])
  (:use clojure.test
        clojure.pprint))

(testing "Baum-Welch algorithm"
  (hmm/train-model tm/ibe-ex-11-model
                   [:good :good :so-so :bad :bad :good :bad :so-so]
                   0.0001))
