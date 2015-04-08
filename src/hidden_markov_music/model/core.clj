(ns hidden-markov-music.model.core
  (:require [hidden-markov-music.hmm          :as hmm]
            [hidden-markov-music.model.train  :as train]
            [hidden-markov-music.model.sample :as sample]
            [hidden-markov-music.util         :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def cli-arguments
  {"train" [train/main
            "Train a model"],
   "sample" [sample/main
             "Sample from a model"]})

(def description
  "Usage: hidden-markov-music model [<options>] <argument> [<args>]")

(def main
  (util/subcommand-parser description
                          util/cli-options-help
                          cli-arguments))

