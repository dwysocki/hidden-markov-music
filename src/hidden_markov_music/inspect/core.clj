(ns hidden-markov-music.inspect.core
  (:require [hidden-markov-music.util :as util]
            [hidden-markov-music.inspect.distribution :as distribution]
            [clojure.string :as string]))

(def cli-arguments
  {"distribution" [distribution/main "Inspect various distributions"]})

(def description
  (->> ["Usage: hidden-markov-music inspect [<options>] <argument> [<args>]"
        ""
        "Perform some form of inspection on a model."]
    (string/join \newline)))

(defn main
  [& args]
  ((util/subcommand-parser description
                           util/cli-options-help
                           cli-arguments)
   args))
