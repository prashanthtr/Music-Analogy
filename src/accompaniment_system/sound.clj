(defn ret-sound [sol]

  (let [randomN (rand-int 2) kick (sample (freesound-path 2086)) tum (sample "src/accompaniment_system/tum.wav" ) ta (sample "src/accompaniment_system/ta.wav" )  ]

    (cond

     (= 'tum sol) tum
     (= 'ta sol) ta
     :else nil
     )

    )

  )

(defn charAtPos [lists pos st]

  (cond

   (empty? lists) nil
   (= pos st) (first lists)
   :else (charAtPos (rest lists) pos (+ st 1))
   )

  )

(defn lengthList [list len]

  (if (empty? list)

    len
    ( lengthList (rest list) (+ len 1))

    )

  )


(defn random-subst [list]

  ( charAtPos list (rand-int (lengthList list 0 )) 0 )
  )


(defn solToLoudness [sol pos]

  (let [addnLoudness (cond
                      (= pos 0) 0.25
                      :else 0
                      )
        hit (charAtPos sol pos 0)
        ]

    (cond

     (>= pos (lengthList sol 0) ) nil
     (= 'ta hit) (cons (+ addnLoudness 0.75) (solToLoudness sol (+ pos 1)) )
     (= 'tum hit) (cons (+ addnLoudness 0.75) (solToLoudness sol (+ pos 1)) )
     (= '. hit) (cons 0 (solToLoudness sol (+ pos 1)) )
     (= 'te hit) (cons 0.375 (solToLoudness sol (+ pos 1)) )
     :else (cons 0 (solToLoudness sol (+ pos 1)) )
     )

    )
  )

(defn differ [sol1 sol2]

  (cond

   (and (empty? sol1) (empty? sol2)) nil
   ;( and (list? sol1 ) (not  (list? sol2))) nil
   ;( and (list? sol2 ) (not  (list? sol1))) nil
   ;( and (list? sol1 ) (list? sol2)) nil
                                        ;( and (not (list? sol1 )) (not (list? sol2)))
   :else (cons (- (first sol1) (first sol2) ) (differ (rest sol1) (rest sol2)))

   )

  )


(defn play-sample
  [samp time vol]
  (at time (stereo-player samp :vol vol)))


(defn newSol []

  ;(println "newsol")
  ;(random-subst  '((ta . tum . tum . ta . ta . tum . tum . ta .)
                   ;(ta . tum . tum . ta ta ta . tum . tum . ta .)
                   ;))
(random-subst  '((ta tum tum ta ta tum tum ta)
                   (ta tum . ta ta tum . ta)
                   ))
  )

; this function will play our sound at whatever tempo we've set our metronome to
(defn looper [nome sol vol]

  (if (empty? sol)

    (let [beat (nome) sol (newSol) vol (solToLoudness sol 0) ]

      (if (= '. (first sol))
      ;(println "sol" sol)
        nil
        (at (nome beat) (play-sample (ret-sound (first sol)) 1 (first vol) )
          ;(stereo-player  :vol (first vol) )
          )

      )
      (apply-at (nome (inc beat)) looper nome (rest sol) (rest vol) [])
      )

    (let [beat (nome) ]

      ;(println "sol2" sol)
                                        ;(at (nome beat) (ret-sound (first sol)) )
      (if (= '. (first sol))

        nil
        (at (nome beat) (play-sample (ret-sound (first sol)) 1 (first vol) )
          ;(stereo-player (ret-sound (first sol)) :vol (first vol) )
          )

        )

      (apply-at (nome (inc beat)) looper nome (rest sol) (rest vol) [])

      )

    )

  )
; implement the latest rules


;; select the suitable ones from among the 60.



;; algo,
;; first bar -> generate exact lead
;; second -> substitute with improv choices at the variable position
;; third -> substitute with improv choices at the variable position
;; fourth -> substitute with improv choices at the variable position