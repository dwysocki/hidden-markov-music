(ns hidden-markov-music.model.signature
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [hidden-markov-music.music.music :as music]
            [hidden-markov-music.music.jfugue :as jfugue]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
   (->> ["Usage: hidden-markov-music signature [<options>]"
         ""
         "Reads a model from standard input, and prints its signature to"
         "standard output."]
      (string/join \newline))))

(def cli-options
  [[nil "--sigma SIGMA"
    "Number of standard deviations from mean to be considered significant."
    :parser util/parse-int]
   ["-h" "--help"]])

(defn main
  [args]
  (let [{:keys [options arguments summary errors]}
        (parse-opts args cli-options)]
    (cond
      (:help options)
      (util/exit 0 (usage summary))

      (not= 0 (count arguments))
      (util/exit 1 (usage summary))

      errors
      (util/exit 1 (util/error-msg errors)))

    (let [model (hmm/stream->model *in*)
          [t-vec o-vec] (hmm/model->signature model)]
      (println (string/join "\t" t-vec))
      (println (string/join "\t" o-vec)))))
