(ns hidden-markov-music.util
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(defmacro assert-args
  "Takes any number of `[boolean message]` pairs, and raises an exception with
  the given message for the first `false` boolean.

  Taken from `clojure.core`, where it is private."
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs)
                      " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (binding [*out* *err*]
    (println msg))
  (System/exit status))

(defn parse-int [x]
  (Integer/parseInt x))

(defn file-extension
  "Returns the extension for the given filename, or nil if there is none."
  [file-name]
  (let [split-name (clojure.string/split file-name #"\.")]
    (when (pos? (count split-name))
      (last split-name))))

(defmacro map-for
  "Map comprehension. Takes a vector of one or more
  binding-form/collection-expr pairs, and returns a nested map of
  collection-expr values to body-expr. Meant as a shorthand for using `zipmap`
  and `for` with the same collection. See examples below:

  ```
  (let [xvals (range 3)
        yvals (range 4 7)]
    (zipmap xvals
      (for [x xvals]
        (* x x)))
    ;;=> {2 4, 1 1, 0 0}
    (map-for [x xvals]
      (* x x))
    ;;=> {2 4, 1 1, 0 0}

    (zipmap xvals
      (for [x xvals]
        (zipmap yvals
          (for [y yvals]
            (* x y)))))
    ;;=> {2 {6 12, 5 10, 4 8}, 1 {6 6, 5 5, 4 4}, 0 {6 0, 5 0, 4 0}}
    (map-for [x xvals
              y yvals]
      (* x y))
    ;;=> {2 {6 12, 5 10, 4 8}, 1 {6 6, 5 5, 4 4}, 0 {6 0, 5 0, 4 0}}
    )
"
  [seq-exprs body-expr]
  (assert-args
    (vector? seq-exprs) "a vector for its binding"
    (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [binding-pairs (->> seq-exprs
                           (partition 2)
                           reverse)]
    (reduce (fn [r [symbol a-seq]]
              (list 'zipmap a-seq
                    (list 'for [symbol a-seq] r)))
            body-expr
            binding-pairs)))

(defn map-vals
  "Maps a function `f` over the values of the map `m`, returning a new map."
  [f m]
  (zipmap (keys m)
          (map f (vals m))))

(defn numbers-almost-equal?
  "Returns true if two numbers are equal to the given decimal place"
  [x y & {:keys [decimal] :or {decimal 6}}]
  (< (Math/abs (- x y))
     (* 0.5
        (Math/pow 10 (- decimal)))))

(defn maps-almost-equal?
  "Returns true if two numeric (potentially nested) maps are equal to the given
  decimal place."
  [x y & {:keys [decimal] :or {decimal 6}}]
  (cond
    ;; x and y are maps
    ;; check that their keys are equal, and that all their values are
    ;; almost equal
    (every? map? [x y])
    (and (= (keys x)
            (keys y))
         (every? identity
                 (map #(maps-almost-equal? %1 %2
                                           :decimal decimal)
                      (vals x)
                      (vals y))))
    ;; x and y are numbers
    ;; check that they are almost equal
    (every? number? [x y])
    (numbers-almost-equal? x y
                           :decimal decimal)
    ;; none of the cases were true, so the maps must not be almost equal
    :else false))

(defn usage-descriptor
  ([description]
     (usage-descriptor description []))
  ([description arguments]
     (fn [options-summary]
       (str description
            "\n"
            "Options:\n"
            options-summary
            (if (seq arguments)
              (str "\n"
                   "\n"
                   "Arguments:\n"
                   (string/join \newline
                                (map (fn [[arg [_ description]]]
                                       (format "    %-20s%s"
                                               arg description))
                                     arguments)))
              "")))))

(def cli-options-help
  [["-h" "--help"]])

(defn subcommand-parser
  [description cli-options cli-arguments]
  (fn [args]
    (let [{:keys [options arguments summary errors]}
          (parse-opts args cli-options
                      :in-order true)
          usage (usage-descriptor description
                                  cli-arguments)]
      (cond
       (:help options)
       (exit 0 (usage summary))

       (not (pos? (count arguments)))
       (exit 1 (usage summary))

       errors
       (exit 1 (error-msg errors)))

      (if-let [sub-command (get cli-arguments
                                (first arguments))]
        (let [[command-fn description] sub-command]
          (command-fn (rest arguments)))
        (exit 1 (usage summary))))))
