;(flute)


(def flute (sample "src/accompaniment_system/tum.wav" ))

(def sol '(tum tum tum . tum tum tum tum))

(def my-pool (overtone.at-at/mk-pool))

;;triggers a hit for the particular duration, does not know the difference between single and double hits

;schedules an event to be played after a particular time
(defn play [sol time]

  (at (+ time (now)) (flute) my-pool)
  (+ time now)

  )

;(every 1000 #(processSol) my-pool)

(show-schedule)

(def kick (sample (freesound-path 2086)))

; setup a tempo for our metronome to use
;(def one-twenty-bpm (metronome 150))

(defn ret-sound [sol]

  (let [randomN (rand-int 2) kick (sample (freesound-path 2086)) tum (sample "src/accompaniment_system/tum.wav" ) ta (sample "src/accompaniment_system/ta.wav" )  ]

    (cond

     (= 'tum sol) (tum)
     (= 'ta sol) (ta)
     :else nil
     )

    )

  )

(defn newSol []

  ;(println "newsol")
  '(ta tum tum ta ta tum tum ta)
  )

; this function will play our sound at whatever tempo we've set our metronome to
(defn looper [nome sol]

  (if (empty? sol)

    (let [beat (nome) sol (newSol) ]

      ;(println "sol" sol)
      (at (nome beat) (ret-sound (first sol)) )
      (apply-at (nome (inc beat)) looper nome (rest sol) [])

      )

    (let [beat (nome) ]

      ;(println "sol2" sol)
      (at (nome beat) (ret-sound (first sol)) )
      (apply-at (nome (inc beat)) looper nome (rest sol) [])

      )

    )

  )
