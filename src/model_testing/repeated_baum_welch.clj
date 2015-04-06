(ns ^:no-doc model-testing.repeated-baum-welch
  (:require [hidden-markov-music.hmm   :as hmm]
            [hidden-markov-music.plots :as plots]
            [incanter.pdf :refer [save-pdf]]
            [clojure.string    :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn parse-int [x]
  (Integer/parseInt x))

(def cli-options
  [["-o" "--output FILE"
    "Name of file to save plot to."]
   [nil "--width W"
    "Width of plot"
    :parse-fn parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--height H"
    "Height of plot"
    :parse-fn parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--observations NUM_OBS"
    "Number of different observation sequences."
    :parse-fn parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--repetitions NUM_REP"
    "Number of times to repeat training on the same observation sequence."
    :parse-fn parse-int
    :validate [#(<= 1 % 1000) "Must be a number between 1 and 1 thousand."]]
   [nil "--iterations NUM_ITER"
    "Number of iterations for each training step."
    :parse-fn parse-int
    :validate [#(<= 1 % 100) "Must be a number between 1 and 1 hundred."]]
   [nil "--state-labels NUM_STATE_LABEL"
    "Number of hidden states in the model."
    :parse-fn parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--observation-labels NUM_OBS_LABEL"
    "Number of observation labels in the model."
    :parse-fn parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   [nil "--length LEN"
    "Length of observation sequences"
    :parse-fn parse-int
    :validate [#(<= 1 % 1000000) "Must be a number between 1 and 1 million."]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Produces a plot of the likelihood of a number of observation"
        "sequences over time."
        ""
        "Usage: repeated-baum-welch [options]"
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

(defn random-observations
  [length labels]
  (repeatedly length #(rand-nth labels)))

(defn multiple-random-observations
  [n length labels]
  (zipmap (range n)
          (repeatedly n #(random-observations length labels))))

(defn -main
  [& args]
  (let [{:keys [options arguments summary errors]}
        (parse-opts args cli-options)]
    (cond
      (:help options)
      (exit 0 (usage summary))

      (not= (count arguments) 0)
      (exit 1 (usage summary))

      errors
      (exit 1 (error-msg errors)))
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
                                                (:repetitions options))
        (save-pdf (:output options)
                  :width  (:width options)
                  :height (:height options))))))
