(ns hidden-markov-music.music.jfugue
  (:import [org.jfugue MidiParser MusicStringRenderer Pattern Player]
           [javax.sound.midi MidiSystem]))

(defn midi->pattern [file]
  (let [player (Player.)]
    (.loadMidi player
               file)))

(defn jfugue->pattern [file]
  (Pattern/loadPattern file))

(defn note? [token]
  (re-matches #"[A-GR].*" token))

(defn midi->notes [file]
  (->> file
       midi->pattern
       .getTokens
       (filter note?)
       #_(filter #(not (.startsWith % "@")))))

(defn jfugue->notes [file]
  (->> file
       jfugue->pattern
       .getTokens))

(defn notes->pattern
  [notes]
  (Pattern. (clojure.string/join " " notes)))

(defn pattern->midi-file
  [file-name pattern]
  (println pattern)
  (.saveMidi (Player.)
             pattern
             (clojure.java.io/file file-name)))

(defn pattern->jfugue-file
  [file-name pattern]
  (println pattern)
  (.savePattern pattern
                (clojure.java.io/file file-name)))


(defn simplify [note]
  (let [[n & _] (.split note "/")] n))
