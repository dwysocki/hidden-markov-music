(ns hidden-markov-music.model.init
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
    (->> ["Initialize a hidden Markov model with an alphabet read from stdin."
          "Alphabet must contain one unique observation symbol per line."
          "Mode may either be 'random' or 'uniform', and determines whether"
          "the probabilities are initialized randomly or uniformly."
          ""
          "Usage: hidden-markov-music init [<options>] <mode>"]
      (string/join \newline))))

(def cli-options
  [["-s" "--states N"
    "Number of hidden states"
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

    (with-open [rdr (clojure.java.io/reader *in*)]
      (let [alphabet (-> rdr
                         ; create a seq out of the lines of the alphabet
                         line-seq
                         ; built a set from that seq
                         set
                         ; add nil to that set, which represents end-of-song
                         (conj nil))
            states (range (:states options))
            mode   (first arguments)
            init-fn (case mode
                      "random" hmm/random-LogHMM
                      (util/exit 1 (str mode " is not a valid mode")))
            model (init-fn states alphabet)]
        (if (hmm/valid-hmm? model)
          (do (pr (into {} model))
              (println))
          (util/exit 1 "Invalid model trained"))))))
