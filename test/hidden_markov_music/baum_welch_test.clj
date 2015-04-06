(ns hidden-markov-music.baum-welch-test
  (:require [hidden-markov-music.hmm         :as hmm]
            [hidden-markov-music.test-models :as tm])
  (:use clojure.test
        clojure.pprint))

(deftest log-baum-welch-algorithm-test
  (testing "logarithmic Baum-Welch algorithm"
   (testing "with Oliver Ibe's Example 11"
     (is (hmm/valid-hmm?
           (hmm/train-model tm/ibe-ex-11-log-model
                            [:good :good :so-so :bad :bad :good :bad :so-so]
                            0.00001))))
   (testing "with Emilio Frazzoli's Baum-Welch example"
     (pprint
      (hmm/LogHMM->HMM
       (hmm/train-model tm/frazzoli-ex-log-model
                        tm/frazzoli-ex-observations
                        0.0001))))))
