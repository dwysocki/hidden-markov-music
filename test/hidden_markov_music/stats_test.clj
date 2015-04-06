(ns hidden-markov-music.stats-test
  (:require [hidden-markov-music.stats :as stats])
  (:use clojure.test))

(deftest stochastic-map-test
  (testing "random stochastic map is stochastic"
    (dotimes [_ 10]
      (is (stats/stochastic-map?
            (stats/random-stochastic-map [:a :b :c :d :e])))))
  (testing "random row-stochastic map is row-stochastic"
    (dotimes [_ 10]
      (is (stats/row-stochastic-map?
            (stats/random-row-stochastic-map [:a :b :c :d :e]
                                             [:A :B :C :D :E]))))))
