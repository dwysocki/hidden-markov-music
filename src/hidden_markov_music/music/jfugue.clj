(ns hidden-markov-music.music.jfugue
  (:import [org.jfugue MusicXmlParser MusicStringRenderer Pattern Player]
           [javax.sound.midi]))

(defn midi->pattern [file]
  (let [player (Player.)]
    (.loadMidi player
               file)))

(defn jfugue->pattern [file]
  (Pattern/loadPattern file))

(defn music-xml->pattern [file]
  (let [stream    (clojure.java.io/input-stream file)
        renderer  (MusicStringRenderer.)]
    (doto (MusicXmlParser.)
      (.addParserListener renderer)
      (.parse file))
    (.getPattern renderer)))

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

(defn music-xml->notes [file]
  (->> file
       music-xml->pattern
       .getTokens))

(defn notes->pattern
  [notes]
  (Pattern. (clojure.string/join " " notes)))

(defn pattern->midi-file
  [file-name pattern]
  (.saveMidi (Player.)
             pattern
             (clojure.java.io/file file-name)))

(defn pattern->jfugue-file
  [file-name pattern]
  (.savePattern pattern
                (clojure.java.io/file file-name)))

#_()

(defn simplify [note]
  (let [[n & _] (.split note "/")] n))
