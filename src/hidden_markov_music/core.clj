(ns hidden-markov-music.core
  (:use [overtone.live])
  (:require [clojure.pprint :refer [pprint]]
            [overtone.inst.piano :refer [piano]]
            [overtone.midi.file :refer [midi-url midi-file]]
            [hidden-markov-music.midi :refer [play-midi parse-midi-events]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string])
  (:gen-class))

(def cli-options
  [["-i" "--input MIDI-FILE" "Input midi file"]
   ["-t" "--track TRACK-NUM" "Track number"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Simply plays the given midi file"
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
  "I don't do a whole lot ... yet."
  [& args]
  (let [{:keys [options arguments errors summary] :as opts}
        (parse-opts args cli-options)]
    (cond
     (:help options)          (exit 0 (usage summary))
     (pos? (count arguments)) (exit 2 (usage summary))
     errors                   (exit 1 (error-msg errors)))

    (let [midi (midi-file (:input options))
          ;; this would be better with transducers
          events (map (comp parse-midi-events :events) (:tracks midi))
          start-time (+ (now) 1000)]
      (doall
       (pmap #(play-midi % piano start-time)
             events)))))
