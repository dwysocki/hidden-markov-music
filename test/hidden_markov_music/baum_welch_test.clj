(ns hidden-markov-music.baum-welch-test
  (:require [hidden-markov-music.hmm         :as hmm]
            [hidden-markov-music.test-models :as tm])
  (:use clojure.test
        clojure.pprint))

(testing "Baum-Welch algorithm"
  (pprint (hmm/train-model tm/ibe-ex-11-model
                           tm/ibe-ex-11-observations
                           0.0001)))
