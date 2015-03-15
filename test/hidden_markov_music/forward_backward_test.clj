(ns hidden-markov-music.forward-backward-test
  "Unit tests for the forward and backward algorithms."
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.test-models :as tm]
            [clojure.pprint :refer [pprint]])
  (:use clojure.test))

(deftest forward-algorithm-test
  (testing "forward algorithm"
    (testing "with Oliver Ibe's Example 11"
      (is (<= 0.0035
              (hmm/likelihood-forward tm/ibe-ex-11-model
                                      tm/ibe-ex-11-observations)
              0.0037)))

    (testing "with deterministic model"
      (is (= (hmm/likelihood-forward tm/deterministic-model
                                     tm/deterministic-certain-observations)
             1.0))
      (is (= (hmm/likelihood-forward tm/deterministic-model
                                     tm/deterministic-impossible-observations)
             0.0)))

    (testing "with 50-50 model"
      (is (= (hmm/likelihood-forward tm/*50-50-model
                                     tm/a-50-50-observations)
             0.5))
      (is (= (hmm/likelihood-forward tm/*50-50-model
                                     tm/b-50-50-observations)
             0.5))
      (is (= (hmm/likelihood-forward tm/*50-50-model
                                     tm/impossible-50-50-observations)
             0.0)))))

(deftest backward-algorithm-test
  (testing "backward algorithm"
    (testing "with Oliver Ibe's Example 11"
      (is (<= 0.0035
              (hmm/likelihood-backward tm/ibe-ex-11-model
                                       tm/ibe-ex-11-observations)
              0.0037)))

    (testing "with deterministic model"
      (is (= (hmm/likelihood-backward tm/deterministic-model
                                      tm/deterministic-certain-observations)
             1.0))
      (is (= (hmm/likelihood-backward tm/deterministic-model
                                      tm/deterministic-impossible-observations)
             0.0)))

    (testing "with 50-50 model"
      (is (= (hmm/likelihood-backward tm/*50-50-model
                                      tm/a-50-50-observations)
             0.5))
      (is (= (hmm/likelihood-backward tm/*50-50-model
                                      tm/b-50-50-observations)
             0.5))
      (is (= (hmm/likelihood-backward tm/*50-50-model
                                      tm/impossible-50-50-observations)
             0.0)))))
