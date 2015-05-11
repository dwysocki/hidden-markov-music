(ns hidden-markov-music.preprocess
  (:require [hidden-markov-music.music.music :as music]
            [hidden-markov-music.music.jfugue :as jfugue]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
    (->> ["Usage: hidden-markov-music preprocess [<options>] <source> <output>"
          ""
          "Preprocesses the source file and outputs a midi file."]
      (string/join \newline))))

(def cli-options
  [[nil "--fix-duration DURATION"
    "Fix the duration of each note to a given value"]
   ["-h" "--help"]])

(defn main
  [args]
  (let [{:keys [options arguments summary errors]}
        (parse-opts args cli-options)]
    (cond
      (:help options)
      (util/exit 0 (usage summary))

      (not= 2 (count arguments))
      (util/exit 1 (usage summary))

      errors
      (util/exit 1 (util/error-msg errors)))

    (let [[input-filename output-filename] arguments
          observations (music/parse-filename-input input-filename)
          preprocessed-observations
          (cond-> observations
            nil identity)]
      (music/parse-filename-output output-filename
                                   preprocessed-observations))))
