(ns hidden-markov-music.midi
  (:require [overtone.live       :refer [at]]
            [overtone.music.time :refer [apply-by now]]))

(defn play-midi
  "Plays a sequence of midi events using the given instrument."
  ([midi-seq inst]
     (play-midi midi-seq inst (now)))
  ([midi-seq inst start-time]
     (play-midi midi-seq inst start-time 1))
  ([midi-seq inst start-time division]
     (when (seq midi-seq)
       (let [{:keys [duration note timestamp velocity]} (first midi-seq)
             midi-seq-rest (next midi-seq)
             next-event (first midi-seq-rest)
             next-timestamp (:timestamp next-event)]

         (at (+ start-time (* timestamp division))
             (inst :note note :velocity velocity
                   :sustain (* duration division)))

         (apply-by (+ start-time (* next-timestamp division))
                   #'play-midi midi-seq-rest inst start-time division [])))))

(defn- time-elapsed
  [next-event prev-event]
  (- (:timestamp next-event)
     (:timestamp prev-event)))

(defn parse-midi-events
  "Extracts the useful information from a sequence of midi events. Combines
  note-on/note-off events into a single event, which plays the note for the
  duration of time between the two events."
  [events]
  (loop [events        events
         active-events {}
         notes         []]
    (if (seq events)
      (let [next-event (first events)]
        (if-let [prev-event (active-events (:channel next-event))]
          (case (:command next-event)
            ;; terminate active note on channel
            :note-off
            (let [duration (time-elapsed next-event
                                         prev-event)
                  note (-> prev-event
                           (assoc :duration duration)
                           (dissoc :command :status :msg))]
              ;; append new note to notes,
              ;; remove the active event which has just been terminated,
              ;; and recur on the remaining events
              (recur (next events)
                     (dissoc active-events (:channel note))
                     (conj notes note)))

            ;; terminate active event on channel and activate current event
            :note-on
            (let [duration (time-elapsed next-event
                                         prev-event)
                  note (-> prev-event
                           (assoc :duration duration)
                           (dissoc :command :status :msg))]
              ;; append new note to notes,
              ;; replace the active event which has just been terminated, with
              ;; the new event,
              ;; and recur on the remaining events
              (recur (next events)
                     (assoc active-events
                       (:channel note)
                       next-event)
                     (conj notes note)))

            ;; event is neither :note-on or :note-off, so we discard it
            (recur (next events)
                   active-events
                   notes))
          (if (= :note-on (:command next-event))
            ;; add current event as the new active event,
            ;; and recur on the remaining events
            (recur (next events)
                   (assoc active-events
                     (:channel next-event)
                     next-event)
                   notes)
            ;; event is not :note-on, and channel is not currently in :note-on
            ;; state, so we discard the current event
            (recur (next events)
                   active-events
                   notes))))
      ;; all events processed, return notes
      notes)))
