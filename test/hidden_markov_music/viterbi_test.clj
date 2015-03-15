(ns hidden-markov-music.viterbi-test
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.test-models :as tm]
            [clojure.pprint :refer [pprint]])
  (:use clojure.test))

(deftest viterbi-algorithm-test
  (testing "viterbi algorithm"
    (testing "with Oliver Ibe's Example 11.1"
      (pprint (hmm/viterbi-path tm/ibe-ex-11-1-model
                                tm/ibe-ex-11-1-observations)))))
