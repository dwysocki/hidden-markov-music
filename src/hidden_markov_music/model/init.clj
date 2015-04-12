(ns hidden-markov-music.model.init
  (:require [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
    (->> ["MAKE DESCRIPTION"
          ""
          "Usage: hidden-markov-music model init [<options>] <mode>"]
      (string/join \newline))))

(def cli-options
  [["-a" "--alphabet FILE"
    "File containing a unique observation symbol on each line"
    :parse-fn (fn [file-name]
                (try
                  (with-open [rdr (clojure.java.io/reader file-name)]
                    (set (line-seq rdr)))
                  (catch java.io.FileNotFoundException e
                    (println "Alphabet file not found")
                    nil)))
    :validate [#(<= 2 (count %)) "Alphabet must contain at least 2 symbols"]]
   ["-s" "--states N"
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

    (let [states (range (:states options))
          mode   (first arguments)
          init-fn (case mode
                    "random" hmm/random-LogHMM
                    (util/exit 1 (str mode " is not a valid mode")))
          model (init-fn states (:alphabet options))]
      (if (hmm/valid-hmm? model)
        (do
          (pr model)
          (println))
        (throw (ex-info "Invalid model trained" {:type :invalid-model}))))))
