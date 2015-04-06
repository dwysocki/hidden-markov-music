(ns hidden-markov-music.viterbi-test
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.test-models :as tm]
            [hidden-markov-music.math :refer [log]]
            [clojure.pprint :refer [pprint]])
  (:use clojure.test))

(deftest viterbi-algorithm-test
  (testing "viterbi algorithm"
    (testing "with Oliver Ibe's Example 11"
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/ibe-ex-11-model
                              tm/ibe-ex-11-observations)]
        (is (<= 2.591e-4 likelihood 2.593e-4))
        (is (tm/ibe-ex-11-viterbi-paths state-sequence))))
    (testing "with deterministic model"
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/deterministic-model
                              tm/deterministic-certain-observations)]
        (is (= likelihood 1.0))
        (is (= state-sequence tm/deterministic-certain-viterbi-path)))
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/deterministic-model
                              tm/deterministic-impossible-observations)]
        (is (= likelihood 0.0))))
    (testing "with 50-50 model"
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/*50-50-model
                              tm/a-50-50-observations)]
        (is (= likelihood 0.5))
        (is (= state-sequence tm/a-50-50-viterbi-path)))
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/*50-50-model
                              tm/b-50-50-observations)]
        (is (= likelihood 0.5))
        (is (= state-sequence tm/b-50-50-viterbi-path))))))

(deftest log-viterbi-algorithm-test
  (testing "logarithmic viterbi algorithm"
    (testing "with Oliver Ibe's Example 11"
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/ibe-ex-11-log-model
                              tm/ibe-ex-11-observations)]
        (is (<= (log 2.591e-4) likelihood (log 2.593e-4)))
        (is (tm/ibe-ex-11-viterbi-paths state-sequence))))
    (testing "with deterministic model"
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/deterministic-log-model
                              tm/deterministic-certain-observations)]
        (is (= likelihood (log 1.0)))
        (is (= state-sequence tm/deterministic-certain-viterbi-path)))
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/deterministic-log-model
                              tm/deterministic-impossible-observations)]
        (is (= likelihood (log 0.0)))))
    (testing "with 50-50 model"
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/*50-50-log-model
                              tm/a-50-50-observations)]
        (is (= likelihood (log 0.5)))
        (is (= state-sequence tm/a-50-50-viterbi-path)))
      (let [{:keys [likelihood state-sequence]}
            (hmm/viterbi-path tm/*50-50-log-model
                              tm/b-50-50-observations)]
        (is (= likelihood (log 0.5)))
        (is (= state-sequence tm/b-50-50-viterbi-path))))))
