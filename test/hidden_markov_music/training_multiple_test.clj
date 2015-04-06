(ns hidden-markov-music.training-multiple-test
  (:require [hidden-markov-music.hmm         :as hmm]
            [hidden-markov-music.plots       :as plots]
            [hidden-markov-music.test-models :as tm])
  (:use clojure.test))

(def multiple-observations
  {:A [:null :null :LA   :LA   :NY   :LA   :null :null :LA   :NY  ],
   :B [:LA   :null :NY   :LA   :null :null :NY   :NY   :null :null],
   :C [:LA   :LA   :NY   :NY   :NY   :null :null :NY   :LA   :LA  ]})

(deftest training-multiple-observations
  (testing "Frazzoli's model with 3 observation sequences and 2 repetitions"
    (plots/training-likelihood-multiple tm/frazzoli-ex-log-model
                                        multiple-observations
                                        100
                                        3)))
