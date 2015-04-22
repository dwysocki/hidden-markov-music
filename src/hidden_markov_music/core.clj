(ns ^:no-doc hidden-markov-music.core
  "The Hidden Markov Music command line interface resides here.
  Currently no such interface exists."
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [hidden-markov-music.util         :as util]
            [hidden-markov-music.alphabet     :as alphabet]
            [hidden-markov-music.model.init   :as init]
            [hidden-markov-music.model.train  :as train]
            [hidden-markov-music.model.sample :as sample]
            [hidden-markov-music.demo.core    :as demo])
  (:gen-class))

(def cli-arguments
  {"alphabet" [alphabet/main "Create an alphabet"],
   "init"     [init/main     "Initialize a model"],
   "train"    [train/main    "Train a model"],
   "sample"   [sample/main   "Sample from a model"],
   "demo"     [demo/main     "Various demonstrations"]})

(def description
  "hidden-markov-music [<options>] <argument> [<args>]")

(defn -main
  [& args]
  ((util/subcommand-parser description
                           util/cli-options-help
                           cli-arguments)
   args))
