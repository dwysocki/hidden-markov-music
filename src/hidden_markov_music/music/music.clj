(ns hidden-markov-music.music.music)

(defn parse-filename
  "Parses music from the filename. If the filename has a known extension,
  it is parsed accordinly, otherwise it returns a seq of the lines of the file.

  Known formats:
    ..."
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))
