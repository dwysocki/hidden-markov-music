(ns ^:no-doc hidden-markov-music.core
  "The Hidden Markov Music command line interface resides here.
  Currently no such interface exists."
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [hidden-markov-music.util :as util]
            [hidden-markov-music.model-demos.repeated-baum-welch
             :as repeated-baum-welch]
            [hidden-markov-music.model-demos.random-sampling
             :as random-sampling])
  (:import [hidden_markov_music.hmm HMM])
  (:gen-class))

(def cli-options
  [["-h" "--help"]])

(def cli-arguments
  {"repeated-baum-welch" repeated-baum-welch/main,
   "random-sampling"     random-sampling/main})

(defn usage [options-summary]
  (->> ["Usage: hidden-markov-music [<options>] <argument> [<args>]"
        ""
        "Options:"
        options-summary
        ""
        "Arguments:"
        (string/join \newline
                     (map #(str "    " %)
                          (keys cli-arguments)))]
       (string/join \newline)))

(defn -main
  [& args]
  (let [{:keys [options arguments summary errors]}
        (parse-opts args cli-options
                    :in-order true)]
    (cond
     (:help options)
     (util/exit 0 (usage summary))

     (not (pos? (count arguments)))
     (util/exit 1 (usage summary))

     errors
     (util/exit 1 (util/error-msg errors)))

    (if-let [sub-command (get cli-arguments (first arguments))]
      (sub-command (rest arguments))
      (util/exit 1 (usage summary)))))
