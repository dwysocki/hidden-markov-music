(ns hidden-markov-music.model.sample
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def cli-arguments
  {"take"
   [(fn [n observations]
      (take (Integer. n) observations))
    "Sample precisely N elements"],
   "until"
   [(fn [o observations]
      (take-while #(not= o %) observations))
    "Sample elements until O is observed"]})

(def usage
  (util/usage-descriptor
    (->> ["Takes a model from standard input and writes a sample observation"
          "sequence to standard output."
          ""
          "Usage: hidden-markov-music model sample [<options>] <command> <arg>"]
      (string/join \newline))
    cli-arguments))

(def cli-options
  [["-h" "--help"]])

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

    (let [model (hmm/stream->model *in*)
          samples (hmm/sample-emissions model)
          [command arg] arguments
          [f _] (get cli-arguments command)]
      (doseq [e (f arg samples)]
        (println e)))))
