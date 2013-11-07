(load-file "src/accompaniment_system/generate.clj")


(defn ret-sound [sol]

  (let [randomN (rand-int 2) kick (sample (freesound-path 2086)) tum (sample "src/accompaniment_system/audio/tum.wav" ) ta (sample "src/accompaniment_system/audio/ta.wav" ) nam (sample "src/accompaniment_system/audio/num.wav" ) dhin (sample "src/accompaniment_system/audio/dhin.wav" ) the (sample "src/accompaniment_system/audio/thi.wav" ) ]

    (cond

     (= 'tum sol) tum
     (= 'ta sol) ta
     (= 'nam sol) nam
     (= 'dhin sol) dhin
     (= 'the sol) the
     :else nil
     )

    )

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
     (= 'ta hit) (cons (+ addnLoudness 0.45) (solToLoudness sol (+ pos 1)) )
     (= 'tum hit) (cons (+ addnLoudness 0.4) (solToLoudness sol (+ pos 1)) )
     (= '. hit) (cons 0 (solToLoudness sol (+ pos 1)) )
     (= 'te hit) (cons (+ addnLoudness 0.8) (solToLoudness sol (+ pos 1)) )
     (= '(ta te) hit) (cons 0.3 (solToLoudness sol (+ pos 1)) )
     (= '(te te) hit) (cons 1 (solToLoudness sol (+ pos 1)) )
      (= '(te ta) hit) (cons 0.3 (solToLoudness sol (+ pos 1)) )
     (= '(ta tum) hit) (cons 0.6 (solToLoudness sol (+ pos 1)) )
     (= '(tum ta) hit) (cons 0.6 (solToLoudness sol (+ pos 1)) )
     (= '(tu tum) hit) (cons 0.75 (solToLoudness sol (+ pos 1)) )
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

; implement the latest rules


;; select the suitable ones from among the 60.



;; algo,
;; first bar -> generate exact lead
;; second -> substitute with improv choices at the variable position
;; third -> substitute with improv choices at the variable position
;; fourth -> substitute with improv choices at the variable position

(defn roundDecimal [num]

  (if (or (float? num) (integer? num))

    (float (/ (round-to ( * num 100 ) 2 ) 100))
    0

    )


  )

(defn anyneg [list]

  (cond

   (empty? list) false
   :else (cond

          (< (first list) 0) true
          :else (or false (anyneg (rest list)))
          )

   )

  )

;complementary hits to contradict the pattern

(defn disp [list]

  (cond

   (true? (anyneg list)) 0
   :else list

   )

  )

(defn subst-cost [sol1 sol2]


  (cond

   (= '. sol1) (cond

                (= 'ta sol2) 0.25
                (= 'tum sol2) 0.25
                (= 'te sol2) 0.5
                (= '(ta te) sol2) 0.2
                (= '(te ta) sol2) 0.2
                (= '. sol2) 0

                )

   ;;continuous substitutions may increase the cost of replacing a tum with ta ri
   (= 'ta sol1) (cond

                 (= 'ta sol2) 0
                 (= 'tum sol2) 0.25
                 (= 'te sol2) 0.5
                 (= '(ta te) sol2) 0.3
                 (= '(te ta) sol2) 0.3
                 :else 0

                 )
   (= 'tum sol1) (cond

                  (= 'ta sol2) 0.6
                  (= 'tum sol2) 0
                  (= 'te sol2) 0.6
                  (= '(ta te) sol2) 0.3
                  (= '(te ta) sol2) 0.3
                  :else 0

                  )
   (= 'te sol1) (cond

                  (= 'ta sol2) 0.3
                  (= 'tum sol2) 0.2
                  (= 'te sol2) 0
                  (= '(ta te) sol2)
                  (= '(te ta) sol2)
                  :else 0
                 )

   (= '(ta te) sol1) (cond

                      (= 'ta sol2) 0.2
                      (= 'tum sol2) 0.2
                      (= 'te sol2) 0.6
                      (= '(ta te) sol2) 0
                      (= '(te ta) sol2) 0.2
                      :else 0
                      )

      (= '(te ta) sol1) (cond

                      (= 'ta sol2) 0.2
                      (= 'tum sol2) 0.2
                      (= 'te sol2) 0.6
                      (= '(ta te) sol2) 0.2
                      (= '(te ta) sol2) 0
                      :else 0
                      )

   )

  )


;; returns the cost of substitution replacement of the substitution
(defn similarity [ sol1 sol2]

  (cond

   (empty? sol1) nil
   (= (first sol1) (first sol2)) (cons 0 (similarity (rest sol1) (rest sol2)))
   :else (cons (subst-cost (first sol1) (first sol2)) (similarity (rest sol1) (rest sol2)) )
   )

  )

(defn find-differ [sol var Loudness]

  (cond

   (empty? var) nil
   :else (do (println (first var) " " (disp (map roundDecimal (differ Loudness (similarity sol (first var)))) ) ) (find-differ sol (rest var) Loudness) )

   )

  )


;(find-differ variations '(1 0.0 0.5 1 0.0 0.5 1 0.5))

;(find-differ '(tum ta tum tum ta tum tum ta) variations '(0.9 0.3 0.5 1 0.25 0.5 1 0.5))



(defn systemv1 [sol]

  (println (apply-rule-map sol ))
  ;(apply-rule-map (mriMap mridangam) )
  ;mridangam
  (apply-rule-map sol)

  )


; this function will play our sound at whatever tempo we've set our metronome to
(defn looper [nome sol vol st]

  (if (>= st (lengthList sol 0))

    (let [beat (nome) st 0]

      (if (= '. (charAtPos sol st 0))
      ;(println "sol" sol)
        nil
        (at (nome beat) (play-sample (ret-sound (charAtPos sol st 0) ) 1 (charAtPos vol st 0)  )
            )

        )
      (apply-at (nome (inc beat)) looper nome sol vol (inc st) [])
      )

    (let [beat (nome) ]

      ;(println "sol2" sol)
                                        ;(at (nome beat) (ret-sound (first sol)) )
      (if (= '. (charAtPos sol st 0))

        nil
        (at (nome beat) (play-sample (ret-sound (charAtPos sol st 0)) 1 (charAtPos vol st 0) )
          ;(stereo-player (ret-sound (first sol)) :vol (first vol) )
          )

        )

      (apply-at (nome (inc beat)) looper nome sol vol (inc st) [])

      )

    )

  )

(looper (metronome 150) '(nam the dhin dhin the dhin dhin the) '(0.9 0.3 0.5 1 0.25 0.5 1 0.5) 0 )

(looper (metronome 150) '(tum ta tum tum ta tum tum ta) '(0.9 0.3 0.5 1 0.25 0.5 1 0.5) 0 )




;; one version of the system that strictly plays only with the forced choices

;; one version that generates substitutions only at the pauses and double hits, when no pauses and double hits, it follows the lead exactly

;; other version, that generates substitutions only at the pauses and double hits, when no pauses and double hits, that introduces random variable note at any position(1st and last position only), generates substitutions and follows

;; other version that substitutes at pauses, double hits and also at the non accented positions, selection of choices is random.

;;only one thing to calibrate -> loudness is same as perceptual loudnes, hmm
