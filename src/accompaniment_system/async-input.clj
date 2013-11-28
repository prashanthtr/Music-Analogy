(definst ding
  [note 60 velocity 100]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (perc 0.1 0.8) :action FREE)]
    (* velocity env snd)))


(defn midi-player [event]
  ;(println "hello world")
  (ding (:note event) (/ (:velocity event) 127.0)))

;(event-debug-on)

(defn logevent [event]
  (println "Note: " (:note event) ", Velocity: " (/ (:velocity event) 127.0))
  )

(def keyboard (midi-in "MidiKeys"))

;(midi-handle-events keyboard #'midi-player)


(on-event [:midi :note-on]
          (fn [e]
            (let [note (:note e)
                  vel  (:velocity e)]
              (println "hello world") (ding note vel)))
          ::keyboard-handler)

;(on-event :midi ::note-on (fn [event] (println "hello note" (:note event))))


;(on-event [:midi :note-on] (fn [{note :note velocity :velocity}]
                            ; (println "Note: " note ", Velocity: " velocity)
;          ::note-printer
;)
