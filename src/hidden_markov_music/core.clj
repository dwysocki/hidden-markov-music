(ns ^:no-doc hidden-markov-music.core
  "The Hidden Markov Music command line interface resides here.
  Currently no such interface exists."
  (:require [clojure.pprint :refer [pprint]]
            [hidden-markov-music.hmm :as hmm]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string])
  (:gen-class))

(def cli-options
  [["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Doesn't do anything ... yet."
        ""
        "Usage: hidden-markov-music [options]"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main
  [& args]
  (pprint (hmm/random-hmm [:rainy :sunny]
                          [:run :clean :shop])))
