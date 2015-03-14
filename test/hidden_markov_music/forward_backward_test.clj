(ns hidden-markov-music.forward-backward-test
  "Unit tests for the forward and backward algorithms."
  (:require [hidden-markov-music.hmm :as hmm]
            [clojure.pprint :refer [pprint]])
  (:use clojure.test)
  (:import [hidden_markov_music.hmm HMM]))

(def ibe-ex-11-1-model
  (HMM. [:sunny :cloudy :rainy]

        [:good :bad :so-so]

        {:sunny  {:sunny  0.5,
                  :cloudy 0.3,
                  :rainy  0.2},
         :cloudy {:sunny  0.4,
                  :cloudy 0.4,
                  :rainy  0.2},
         :rainy  {:sunny  0.2,
                  :cloudy 0.4,
                  :rainy  0.4}}

        {:sunny  {:good  0.6,
                  :bad   0.1,
                  :so-so 0.3},
         :cloudy {:good  0.3,
                  :bad   0.2,
                  :so-so 0.5},
         :rainy  {:good  0.1,
                  :bad   0.6,
                  :so-so 0.3}}

        {:sunny  (/ 3.0)
         :cloudy (/ 3.0),
         :rainy  (/ 3.0)}))

(def ibe-ex-11-1-observations
  [:good :good :so-so :bad :bad])

(def deterministic-model
  (HMM. [:A :B :C]

        [:a :b :c]

        {:A {:A 0.0,
             :B 1.0,
             :C 0.0},
         :B {:A 0.0,
             :B 0.0,
             :C 1.0},
         :C {:A 1.0,
             :B 0.0,
             :C 0.0}}

        {:A {:a 1.0,
             :b 0.0,
             :c 0.0},
         :B {:a 0.0,
             :b 1.0,
             :c 0.0},
         :C {:a 0.0,
             :b 0.0,
             :c 1.0}}

        {:A 1.0,
         :B 0.0,
         :C 0.0}))

(def deterministic-certain-observations
  [:a :b :c :a :b :c])

(def deterministic-impossible-observations
  [:c :b :c :c :a])

(def *50-50-model
  (HMM. [:A :B]

        [:a :b]

        {:A {:A 0.0,
             :B 1.0},
         :B {:A 1.0,
             :B 0.0}}

        {:A {:a 1.0,
             :b 0.0},
         :B {:a 0.0,
             :b 1.0}}

        {:A 0.5,
         :B 0.5}))

(def a-50-50-observations
  [:a :b :a :b :a :b])

(def b-50-50-observations
  [:b :a :b :a :b])

(def impossible-50-50-observations
  [:a :a :b :a :b])

(deftest forward-algorithm-test
  (testing "forward algorithm"
    (testing "with Oliver Ibe's Example 11.1"
      (is (<= 0.0035
              (hmm/forward-likelihood ibe-ex-11-1-model
                                      ibe-ex-11-1-observations)
              0.0037)))

    (testing "with deterministic model"
      (is (= (hmm/forward-likelihood deterministic-model
                                     deterministic-certain-observations)
             1.0))
      (is (= (hmm/forward-likelihood deterministic-model
                                     deterministic-impossible-observations)
             0.0)))

    (testing "with 50-50 model"
      (is (= (hmm/forward-likelihood *50-50-model
                                     a-50-50-observations)
             0.5))
      (is (= (hmm/forward-likelihood *50-50-model
                                     b-50-50-observations)
             0.5))
      (is (= (hmm/forward-likelihood *50-50-model
                                     impossible-50-50-observations)
             0.0)))))

(deftest backward-algorithm-test
  (testing "backward algorithm"
    (testing "with Oliver Ibe's Example 11.1"
      (is (<= 0.0035
              (hmm/backward-likelihood ibe-ex-11-1-model
                                       ibe-ex-11-1-observations)
              0.0037)))

    (testing "with deterministic model"
      (is (= (hmm/backward-likelihood deterministic-model
                                      deterministic-certain-observations)
             1.0))
      (is (= (hmm/backward-likelihood deterministic-model
                                      deterministic-impossible-observations)
             0.0)))

    (testing "with 50-50 model"
      (is (= (hmm/backward-likelihood *50-50-model
                                      a-50-50-observations)
             0.5))
      (is (= (hmm/backward-likelihood *50-50-model
                                      b-50-50-observations)
             0.5))
      (is (= (hmm/backward-likelihood *50-50-model
                                      impossible-50-50-observations)
             0.0)))))
