(ns ^:no-doc hidden-markov-music.core
  "The Hidden Markov Music command line interface resides here.
  Currently no such interface exists."
  (:require [clojure.string :as string]
            [hidden-markov-music.util             :as util]
            [hidden-markov-music.alphabet         :as alphabet]
            [hidden-markov-music.preprocess       :as preprocess]
            [hidden-markov-music.model.count      :as count]
            [hidden-markov-music.model.likelihood :as likelihood]
            [hidden-markov-music.model.init       :as init]
            [hidden-markov-music.model.train      :as train]
            [hidden-markov-music.model.auto-train :as auto-train]
            [hidden-markov-music.model.sample     :as sample]
            [hidden-markov-music.model.signature  :as signature]
            [hidden-markov-music.demo.core        :as demo])
  (:gen-class))

(def cli-arguments
  {"alphabet"
   [alphabet/main "Create an alphabet"],

   "preprocess"
   [preprocess/main "Preprocess a music file"]
   
   "count"
   [count/main "Count an attribute of a model"],

   "likelihood"
   [likelihood/main "Determine the likelihood of a model producing a song"]

   "init"
   [init/main "Initialize a model"],

   "train"
   [train/main "Train an existing model"],

   "auto-train"
   [auto-train/main "Train a new model automatically"]

   "sample"
   [sample/main "Sample from a model"],

   "signature"
   [signature/main "Obtain a model's signature"]

   "demo"
   [demo/main "Various demonstrations"]})

(def description
  (->> ["hidden-markov-music [<options>] <argument> [<args>]"
        ""
        "Perform an operation on a musical hidden Markov model."]
    (string/join \newline)))

(defn -main
  [& args]
  ((util/subcommand-parser description
                           util/cli-options-help
                           cli-arguments)
   args))
