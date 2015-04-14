(ns hidden-markov-music.music.jfugue
  (:import [org.jfugue MidiParser MusicStringRenderer Player]
           [javax.sound.midi MidiSystem]))

(defn midi->pattern [stream]
  (let [sequencer (doto (MidiSystem/getSequencer)
                    (.setSequence stream))
        parser (MidiParser.)
        renderer (MusicStringRenderer.)]
    (.addParserListener parser renderer)
    (.parse parser (.getSequence sequencer))
    (.getPattern renderer)))

(defn note? [token]
  (re-matches #"[A-GR].*" token))

(defn midi->notes [stream]
  (->> stream
       midi->pattern
       .getTokens
       (filter note?)))

(defn notes->pattern
  [notes]
  (clojure.string/join " " notes))

(defn pattern->midi-file
  [file-name pattern]
  (.saveMidi (Player.)
             pattern
             (clojure.java.io/file file-name)))

(defn simplify [note]
  (let [[n & _] (.split note "/")] n))
