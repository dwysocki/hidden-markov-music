(ns hidden-markov-music.model.sample
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.music.music :as music]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
   (->> ["Usage: hidden-markov-music sample [<options>] <file>"
         ""
         "Reads a model from standard input and generates a song, and saves it"
         "to the given file."]
      (string/join \newline))))

(def cli-options
  [[nil "--limit N" "Maximum length of observation sequence."
    :default 1000000
    :parse-fn util/parse-int
    :validate [#(< % 0x10000) "Must be an integer between 0 and 65536"]]
   ["-h" "--help"]])

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

    (let [model (hmm/stream->model *in*)
          notes (->> model
                     hmm/sample-emissions
                     (take-while identity)
                     (take (:limit options)))
          file-name (first arguments)]
      (music/parse-filename-output file-name notes))))
