(ns hidden-markov-music.model.train
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [hidden-markov-music.music.music :as music]
            [hidden-markov-music.music.jfugue :as jfugue]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
   (->> ["Usage: hidden-markov-music train [<options>] <file>"
         ""
         "Reads a model from standard input, and the given music file."
         "Writes an updated model to standard output, as obtained by the"
         "Baum-Welch algorithm."
         "Tries to recognize the file format from its extension. If format is"
         "unsupported, assumes file is a text file where each line contains"
         "one observation symbol."]
      (string/join \newline))))

(def cli-options
  [["-i" "--max-iter N"
    "Maximum number of Baum-Welch training iterations"
    :default 100
    :parse-fn util/parse-int
    :validate [#(< 0 % 0x100000) "Must be an integer between 0 and 65536"]]
   ["-d" "--decimal N"
    "Number of decimal places to measure convergence to"
    :default 10
    :parse-fn util/parse-int
    :validate [#(< 0 % 0x100000) "Must be an integer between 0 and 65536"]]
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

    (let [observation-filename (first arguments)
          model (hmm/stream->model *in*)
          observations (music/parse-filename-input observation-filename)
          observations (lazy-cat observations [nil])]
      (pr (hmm/train-model model observations
                           :decimal  (:decimal  options)
                           :max-iter (:max-iter options)))
      (println))))
