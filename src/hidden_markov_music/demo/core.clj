(ns hidden-markov-music.demo.core
  (:require [hidden-markov-music.demo.random-sampling
             :as random-sampling]
            [hidden-markov-music.demo.repeated-baum-welch
             :as repeated-baum-welch]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def cli-arguments
  {"repeated-baum-welch"
   [repeated-baum-welch/main
    "Visualize repeated Baum-Welch training"],
   "random-sampling"
   [random-sampling/main
    "Randomly sample from some contrived HMMs"]})

(def description
  "hidden-markov-music demo [<options>] <argument> [<args>]")

(def main
  (util/subcommand-parser description
                          util/cli-options-help
                          cli-arguments))
