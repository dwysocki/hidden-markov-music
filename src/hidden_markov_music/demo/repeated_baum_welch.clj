(ns ^:no-doc hidden-markov-music.demo.repeated-baum-welch
  (:require [hidden-markov-music.hmm   :as hmm]
            [hidden-markov-music.math  :refer [exp log]]
            [hidden-markov-music.plots :as plots]
            [hidden-markov-music.util  :as util]
            [incanter.pdf :refer [save-pdf]]
            [clojure.string    :as string]
            [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-o" "--output FILE"
    "Name of file to save plot to."]
   [nil "--width W"
    "Width of plot"
    :default 500
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--height H"
    "Height of plot"
    :default 300
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--observations NUM_OBS"
    "Number of different observation sequences."
    :default 3
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--repetitions NUM_REP"
    "Number of times to repeat training on the same observation sequence."
    :default 3
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000) "Must be a number between 1 and 1 thousand."]]
   [nil "--iterations NUM_ITER"
    "Number of iterations for each training step."
    :default 20
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--state-labels NUM_STATE_LABEL"
    "Number of hidden states in the model."
    :default 2
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--observation-labels NUM_OBS_LABEL"
    "Number of observation labels in the model."
    :default 2
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--length LEN"
    "Length of observation sequences"
    :default 10
    :parse-fn util/parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   ["-h" "--help"]])

(def usage
  (util/usage-descriptor
    (->> ["Produces a plot of the likelihood of a number of observation"
          "sequences over time."
          ""
          "Usage: hidden-markov-music repeated-baum-welch [options]"]
      (string/join \newline))))

(defn random-observations
  [length labels]
  (repeatedly length #(rand-nth labels)))

(defn multiple-random-observations
  [n length labels]
  (zipmap (range n)
          (repeatedly n #(random-observations length labels))))

(defn main
  [args]
  (let [{:keys [options arguments summary errors]}
        (parse-opts args cli-options)]
    (cond
      (:help options)
      (util/exit 0 (usage summary))

      (not= (count arguments) 0)
      (util/exit 1 (usage summary))

      errors
      (util/exit 1 (util/error-msg errors)))
    (let [state-labels       (range (:state-labels       options))
          observation-labels (range (:observation-labels options))
          initial-model      (hmm/random-LogHMM state-labels
                                                observation-labels)
          multiple-observations
          (multiple-random-observations (:observations options)
                                        (:length       options)
                                        observation-labels)]
      (doto (plots/training-likelihood-multiple initial-model
                                                multiple-observations
                                                (:iterations  options)
                                                (:repetitions options)
                                                :scaling exp)
        (save-pdf (:output options)
                  :width  (:width options)
                  :height (:height options))))))
