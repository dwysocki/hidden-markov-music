(ns hidden-markov-music.test-models
  (:require [hidden-markov-music.hmm :as hmm])
  (:import [hidden_markov_music.hmm HMM]))

;; HMM taken from Example 11 in Oliver Ibe's
;; "Markov Processes for Stochastic Modeling"
(def ibe-ex-11-model
  (HMM. [:sunny :cloudy :rainy]

        [:good :bad :so-so]

        {:sunny  (/ 3.0),
         :cloudy (/ 3.0),
         :rainy  (/ 3.0)}

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
                  :so-so 0.3}}))

(def ibe-ex-11-log-model
  (hmm/HMM->LogHMM ibe-ex-11-model))

(def ibe-ex-11-observations
  [:good :good :so-so :bad :bad])

(def ibe-ex-11-viterbi-paths
  #{[:sunny :sunny :sunny  :rainy :rainy]
    [:sunny :sunny :cloudy :rainy :rainy]})


;; models taken from Larry Moss' Baum-Welch examples
(def moss-ex-1-model
  (HMM. [:s :t]

        [:A :B]

        {:s 0.85,
         :t 0.15}

        {:s {:s 0.3,
             :t 0.7},
         :t {:s 0.1,
             :t 0.9}},

        {:s {:A 0.4,
             :B 0.6},
         :t {:A 0.5,
             :B 0.5}}))

(def moss-ex-1-log-model
  (hmm/HMM->LogHMM moss-ex-1-model))


;; model taken from Emilio Frazzoli's Baum-Welch example
(def frazzoli-ex-model
  (HMM. [:LA :NY]

        [:LA :NY :null]

        {:LA 0.5,
         :NY 0.5}

        {:LA {:LA 0.5,
              :NY 0.5},
         :NY {:LA 0.5,
              :NY 0.5}}

        {:LA {:LA   0.4,
              :NY   0.1,
              :null 0.5},
         :NY {:LA   0.1,
              :NY   0.5,
              :null 0.4}}))

(def frazzoli-ex-log-model
  (hmm/HMM->LogHMM frazzoli-ex-model))

(def frazzoli-ex-observations
  [:null :LA   :LA   :null :NY   :null :NY   :NY   :NY   :null
   :NY   :NY   :NY   :NY   :NY   :null :null :LA   :LA   :NY  ])

(def frazzoli-ex-trained-model
  (HMM. [:LA :NY]

        [:LA :NY :null]

        {:LA 1.0,
         :NY 0.0}

        {:LA {:LA 0.6909
              :NY 0.3091},
         :NY {:LA 0.0934
              :NY 0.9066}}

        {:LA {:LA   0.5807
              :NY   0.0010
              :null 0.4183},
         :NY {:LA   0.0000
              :NY   0.7621
              :null 0.2379}}))

;; fully deterministic HMM, whose states must be
;; :A -> :B -> :C -> :A -> ...
;; and whose emissions must be
;; :a -> :b -> :c -> :a -> ...
(def deterministic-model
  (HMM. [:A :B :C]

        [:a :b :c]

        {:A 1.0,
         :B 0.0,
         :C 0.0}

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
             :c 1.0}}))

(def deterministic-log-model
  (hmm/HMM->LogHMM deterministic-model))

(def deterministic-certain-observations
  [:a :b :c :a :b :c])

(def deterministic-impossible-observations
  [:c :b :c :c :a])

(def deterministic-certain-viterbi-path
  [:A :B :C :A :B :C])

;; model which can begin in one of two states, but from there is deterministic
(def *50-50-model
  (HMM. [:A :B]

        [:a :b]

        {:A 0.5,
         :B 0.5}

        {:A {:A 0.0,
             :B 1.0},
         :B {:A 1.0,
             :B 0.0}}

        {:A {:a 1.0,
             :b 0.0},
         :B {:a 0.0,
             :b 1.0}}))

(def *50-50-log-model
  (hmm/HMM->LogHMM *50-50-model))

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
