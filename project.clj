(defproject hidden-markov-music "0.1.5-SNAPSHOT"
  :description "Generate original musical scores by means of a hidden Markov
                model."
  :url "https://github.com/dwysocki/hidden-markov-music"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :codox {:src-dir-uri
          "https://github.com/dwysocki/hidden-markov-music/tree/master/"
          :src-linenum-anchor-prefix "L"
          :defaults {:doc/format :markdown}}
  :dependencies [[org.clojure/clojure      "1.6.0"]
                 [org.clojure/tools.cli    "0.3.1"]
                 [org.clojure/tools.reader "0.9.1"]
                 [jfugue                   "4.0.3"]
                 [incanter                 "1.5.5"]]
  :main hidden-markov-music.core)
