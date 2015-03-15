(ns hidden-markov-music.test-models
  (:import [hidden_markov_music.hmm HMM]))

(def ibe-ex-11-model
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

(def ibe-ex-11-observations
  [:good :good :so-so :bad :bad])

(def ibe-ex-11-viterbi-paths
  #{[:sunny :sunny :sunny  :rainy :rainy]
    [:sunny :sunny :cloudy :rainy :rainy]})

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

(def deterministic-certain-viterbi-path
  [:A :B :C :A :B :C])

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
  [:b :a :b :a :b :a])

(def impossible-50-50-observations
  [:a :a :b :a :b])

(def a-50-50-viterbi-path
  [:A :B :A :B :A :B])

(def b-50-50-viterbi-path
  [:B :A :B :A :B :A])
