(ns hidden-markov-music.music.music
  (:require [hidden-markov-music.music.jfugue :as jfugue]
            [hidden-markov-music.util :as util]))

(defn midi?
  "Returns true if the extension is a recognized midi extension.
  Only recognizes .mid and .midi extensions, and is case insensitive."
  [extension]
  (when extension
    (let [extension-lower (clojure.string/lower-case extension)]
      (or (= extension-lower "mid")
          (= extension-lower "midi")))))


(defn parse-filename-input
  "Parses music from the filename. If the filename has a known extension,
  it is parsed accordinly, otherwise it returns a seq of the lines of the file.

  Known formats:
    midi"
  [file-name]
  (let [extension (util/file-extension file-name)]
    (cond
      (midi? extension)
      (-> file-name
          clojure.java.io/input-stream
          jfugue/midi->notes)

      :else
      (with-open [rdr (clojure.java.io/reader file-name)]
        (doall (line-seq rdr))))))

(defn parse-filename-output
  "Saves the output to a file, formatting it according to the extension in the
  filename."
  [file-name notes]
  (let [extension (util/file-extension file-name)]
    (cond
      (midi? extension)
      (->> notes
           jfugue/notes->pattern
           (jfugue/pattern->midi-file file-name))

      :else
      (with-open [wrtr (clojure.java.io/writer file-name)]
        (doseq [n notes]
          (.write wrtr n)
          (.write wrtr \newline))))))
