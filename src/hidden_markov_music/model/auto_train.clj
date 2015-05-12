(ns hidden-markov-music.model.auto-train
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [hidden-markov-music.music.music :as music]
            [hidden-markov-music.music.jfugue :as jfugue]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
   (->> ["Usage: hidden-markov-music auto-train [<options>] <file> <mode>"
         ""
         "Reads a music file, and trains a model by the Baum-Welch algorithm."
         "Selects the number of states by searching over a user-provided range"
         "for the simplest model with the maximum likelihood."
         ""
         "Tries to recognize the file format from its extension. If format is"
         "unsupported, assumes file is a text file where each line contains"
         "one observation symbol."]
      (string/join \newline))))

(def cli-options
  [["-m" "--mode MODE"
    "Model initialization mode (uniform or random)"
    :default :uniform
    :parse-fn keyword
    :validate [#{:uniform :random} "Must be uniform or random"]]
   [nil "--min MIN"
    "Minimum number of states in search range."
    :default 1
    :parse-fn util/parse-int
    :validate [#(< 0 % 0x100000) "Must be an integer between 0 and 65536"]]
   [nil "--max MAX"
    "Maximum number of states in search range."
    :default 100
    :parse-fn util/parse-int
    :validate [#(< 0 % 0x100000) "Must be an integer between 0 and 65536"]]
   ["-b" "--bins N"
    "Number of bins to divide search space into at each iteration."
    :default 10
    :parse-fn util/parse-int
    :validate [#(< 0 % 0x100000) "Must be an integer between 0 and 65536"]]
   ["-i" "--max-iter N"
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

    (let [[observation-filename mode] arguments
          observations (music/parse-filename-input observation-filename)
          observations (lazy-cat observations [nil])
          ;; make alphabet here
          alphabet (set observations)]
      (pr (hmm/auto-train-model observations
                                alphabet
                                :mode     (:mode     options)
                                :min      (:min      options)
                                :max      (:max      options)
                                :bins     (:bins     options)
                                :decimal  (:decimal  options)
                                :max-iter (:max-iter options)))
      (println))))
