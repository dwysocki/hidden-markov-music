(ns hidden-markov-music.forward-algorithm-test
  (:require [hidden-markov-music.hmm :as hmm]
            [clojure.pprint :refer [pprint]])
  (:use clojure.test)
  (:import [hidden_markov_music.hmm HMM]))

(deftest forward-algorithm-test
  (testing "forward algorithm"
    (testing "with Oliver Ibe's Example 11.1"
      (let [model (HMM. [:sunny :cloudy :rainy]

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
                         :rainy  (/ 3.0)})

            observations [:good :good :so-so :bad :bad]

            likelihood (hmm/observation-likelihood model observations)]
        (is (<= 0.0035 likelihood 0.0037))))

    (testing "with deterministic model"
      (let [model (HMM. [:A :B :C]

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
                         :C 0.0})

            certain-observations [:a :b :c :a :b :c]
            impossible-observations [:c :b :c :c :a]]

        (is (= (hmm/observation-likelihood model certain-observations)
               1.0))
        (is (= (hmm/observation-likelihood model impossible-observations)
               0.0))))

    (testing "with 50-50 model"
      (let [model (HMM. [:A :B]

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
                         :B 0.5})
            a-observations [:a :b :a :b :a :b]
            b-observations [:b :a :b :a :b]
            impossible-observations [:a :a :b :a :b]]

        (is (= (hmm/observation-likelihood model a-observations)
               0.5))
        (is (= (hmm/observation-likelihood model b-observations)
               0.5))
        (is (= (hmm/observation-likelihood model impossible-observations)
               0.0))))))
