(ns hidden-markov-music.model.count
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
   (->> ["Usage: hidden-markov-music count [<options>] <states|observations>"
         ""
         "Counts the number of states or observations in a model read from"
         "stdin, and prints the number to stdout."]
      (string/join \newline))))

(def cli-options
  util/cli-options-help)


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

    (with-open [rdr (clojure.java.io/reader *in*)]
      (let [attribute (first arguments)
            model (hmm/stream->model *in*)]
        (case attribute
          "states"
          (println (hmm/count-states model))

          "observations"
          (println (hmm/count-observations model))

          (util/exit 1 (usage summary)))))))
