(ns hidden-markov-music.log-hmm-transformation-test
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util])
  (:use clojure.test))

(deftest inverse-transform-test
  (testing "HMM->LogHMM inverse"
    (let [hmm-initial     (hmm/random-HMM
                            [:A :B :C]
                            [:a :b :c])
          hmm-transformed (hmm/HMM->LogHMM hmm-initial)
          hmm-final       (hmm/LogHMM->HMM hmm-transformed)]
      (is (hmm/hmms-almost-equal? hmm-initial hmm-final
                                  :decimal 15))))
  (testing "LogHMM->HMM inverse"
    (let [hmm-initial     (hmm/random-LogHMM
                            [:A :B :C]
                            [:a :b :c])
          hmm-transformed (hmm/LogHMM->HMM hmm-initial)
          hmm-final       (hmm/HMM->LogHMM hmm-transformed)]
      (is (hmm/hmms-almost-equal? hmm-initial hmm-final
                                  :decimal 15)))))
