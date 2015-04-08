(ns hidden-markov-music.demo.random-sampling
  (:require [clojure.pprint :refer [pprint]]
            [hidden-markov-music.hmm :as hmm]
            [hidden-markov-music.stats :as stats]
            [hidden-markov-music.util  :as util]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string])
  (:import [hidden_markov_music.hmm HMM]))

(def random-weather-model
  (hmm/random-HMM [:rainy :sunny]
                  [:run :clean :shop]))

(def song-states
  [:beginning :middle :chorus :finale :end])

(def song-notes
  [:A :B :C :D :E :F :G])

(def song-model
  (HMM. song-states
        song-notes
        {:beginning 1.0,
         :middle    0.0,
         :chorus    0.0,
         :finale    0.0,
         :end       0.0}
        {:beginning {:beginning 0.8,
                     :middle    0.2,
                     :chorus    0.0,
                     :finale    0.0,
                     :end       0.0},
         :middle    {:beginning 0.0,
                     :middle    0.5,
                     :chorus    0.4,
                     :finale    0.1,
                     :end       0.0},
         :chorus    {:beginning 0.0,
                     :middle    0.2,
                     :chorus    0.8,
                     :finale    0.0,
                     :end       0.0}
         :finale    {:beginning 0.0,
                     :middle    0.0,
                     :chorus    0.0,
                     :finale    0.8,
                     :end       0.2}
         :end       {:beginning 0.0,
                     :middle    0.0,
                     :chorus    0.0,
                     :finale    0.0,
                     :end       1.0}}
        (stats/random-row-stochastic-map song-states song-notes)))

(def models
  {"weather" random-weather-model,
   "song"    song-model})

(def usage
  (util/usage-descriptor
    (->> ["Produces random emissions and states from walking an HMM."
          ""
          "Usage: hidden-markov-music random-sampling [options]"]
      (string/join \newline))))

(def cli-options
  [["-m" "--model MODEL" "Name of the model"
    :default "weather"
    :validate [#{"weather" "song"}
               (str "Must be one of [weather, song]")]]
   ["-n" "--number N" "Number of weather events to sample"
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-h" "--help"]])

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

    (let [model (-> (:model options)
                    models)
          states (hmm/sample-states model)
          emissions (hmm/sample-emissions model states)]
      (println "MODEL:")
      (pprint model)
      (println)
      (println "SAMPLE:")
      (doall
       (case (:model options)
         "song"
         (map (fn [state emission]
                (println (str "S = " state ", E = " emission)))
              (take-while (fn [state] (not= state :end))
                          states)
              emissions)
         "weather"
         (take (:number options)
               (map (fn [state emission]
                      (println (str "S = " state ", E = " emission)))
                    states
                    emissions))
         nil))
      nil)))
