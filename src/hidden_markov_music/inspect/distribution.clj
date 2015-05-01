(ns hidden-markov-music.inspect.distribution
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
   (->> ["Usage: hidden-markov-music inspect distribution [<options>] <output>"
         ""
         "Reads a model from standard input and outputs plots of the"
         "distributions specified to the given output directory."]
      (string/join \newline))))

(def cli-options
  [[nil "--emissions"
    "Plots the distribution of emissions in each state."]
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
          output-dir (first arguments)]
      ;; ...
      nil)))
