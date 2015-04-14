(ns ^:no-doc hidden-markov-music.core
  "The Hidden Markov Music command line interface resides here.
  Currently no such interface exists."
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [hidden-markov-music.util :as util]
            [hidden-markov-music.alphabet :as alphabet]
            [hidden-markov-music.model.core :as model]
            [hidden-markov-music.demo.core  :as demo])
  (:gen-class))

(def cli-arguments
  {"alphabet" [alphabet/main
               "Create an alphabet"]
   "model" [model/main
            "Perform an operation on a model"]
   "demo"  [demo/main
            "Various demonstrations"]})

(def description
  "hidden-markov-music [<options>] <argument> [<args>]")

(defn -main
  [& args]
  ((util/subcommand-parser description
                           util/cli-options-help
                           cli-arguments)
   args))
