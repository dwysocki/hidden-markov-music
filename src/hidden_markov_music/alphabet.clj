(ns hidden-markov-music.alphabet
  (:require [hidden-markov-music.music.music :as music]
            [hidden-markov-music.music.jfugue :as jfugue]
            [hidden-markov-music.util :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def usage
  (util/usage-descriptor
    (->> ["Print the alphabet of the observation file."
          ""
          "Usage: hidden-markov-music alphabet [<options>] <file>"]
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

    (let [file-name (first arguments)
          alphabet (music/parse-filename-input file-name)
          alphabet (set alphabet)]
      (doseq [sym alphabet]
        (println sym)))))
