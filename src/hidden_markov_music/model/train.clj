(ns hidden-markov-music.model.train
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [hidden-markov-music.music.music :as music]
            [hidden-markov-music.music.jfugue :as jfugue]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
    (->> ["Takes a model from standard input and a file containing"
          "observations, and writes an updated model to standard output."
          "Trains the model using the Baum-Welch algorithm. If the"
          "observation file extension is recognized, the file is parsed"
          "appropriately, otherwise each line of the file is treated as an"
          "observation symbol."
          ""
          "Usage: hidden-markov-music train [<options>] <observations>"]
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
   [nil "--simplify"
    "Extract only the most simple information from the notes."
    :default false]
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
          observations (if (:simplify options)
                         (map jfugue/simplify observations)
                         observations)
          observations (lazy-cat observations [nil])]
      (pr (hmm/train-model model observations
                           :decimal  (:decimal  options)
                           :max-iter (:max-iter options)))
      (println))))
