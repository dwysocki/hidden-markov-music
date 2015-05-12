(ns hidden-markov-music.model.likelihood
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [hidden-markov-music.music.music :as music]
            [hidden-markov-music.music.jfugue :as jfugue]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
   (->> ["Usage: hidden-markov-music likelihood [<options>] <file>"
         ""
         "Reads a model from standard input, and the given music file."
         "Writes the likelihood of observing that song to standard output,"
         "as obtained by the forward algorithm."
         ""
         "Tries to recognize the file format from its extension. If format is"
         "unsupported, assumes file is a text file where each line contains"
         "one observation symbol."]
      (string/join \newline))))

(def cli-options
  [["-h" "--help"]])

(defn main
  [args]
  (let [{:keys [options arguments summary errors]}
        (parse-opts args cli-options)]
    (cond
      (:help options)
      (util/exit 0 (usage summary))

      (not= 1 (count arguments))
      (util/exit 1 (usage summary))

      errors
      (util/exit 1 (util/error-msg errors)))

    (let [observation-filename (first arguments)
          model (hmm/stream->model *in*)
          observations (music/parse-filename-input observation-filename)
          observations (lazy-cat observations [nil])]
      (println (hmm/likelihood-forward model observations)))))
