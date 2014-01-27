(defn ret-sound [sol]

  ;(println sol)
  (let [randomN (rand-int 2) kick (load-sample (freesound-path 2086)) tum (load-sample "src/accompaniment_system/audio/tum.wav" ) ta (load-sample "src/accompaniment_system/audio/ta.wav" ) num (load-sample "src/accompaniment_system/audio/num.wav" ) dhin (load-sample "src/accompaniment_system/audio/dhin.wav" ) thi (load-sample "src/accompaniment_system/audio/thi.wav" ) te (load-sample "src/accompaniment_system/audio/te.wav" ) song (load-sample "src/accompaniment_system/audio/song.wav") ]

    (cond

     (= 'tum sol) tum
     (= 'ta sol) ta
     (= 'num sol) num
     (= 'dhin sol) dhin
     (= 'thi sol) thi
     (= 'te sol) te
     (= 'song sol) song
     :else nil
     )

    )

  )

(defn play-sample
  [samp vol]
  (if (= nil samp)
    nil
    (stereo-player samp :vol vol)
    )

  )


(defn pat-vol [pat vol]

  (cond

   (empty? pat) nil
   (list? (first pat)) (concat (list (first vol) (first vol)) (pat-vol (rest pat) (rest vol)) )
   :else (cons (first vol) (pat-vol (rest pat) (rest vol)))

   )

  )


;;interprets the hits on the pattern based as single or double and plays them
(defn pattern-play [tempo pat forced-vol]

  (let [vol (pat-vol pat forced-vol) tmp pat speed (list2Vec (time-hit tempo tmp)) schedule (create-schedule speed tempo 0 0) pattern (list2Vec (d2Sr tmp))  ]

  (println pat vol)

    (dorun (map-indexed (fn [i n] (at (+ (now) (nth schedule i)) (play-sample (ret-sound n) (nth vol i)))) pattern))
    )

  )

(def mr '(num . dhin (num thi) num . dhin thi . . thi ri . thi num dhin thi ri . thi num dhin thi ri . thi num dhin ))
(def loud '(0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9))


(defn mrMap [mSol]


   (cond

    (empty? mSol) nil
    (= '. (first mSol)) (cons '. (mrMap (rest mSol)))
    (or (= 'dham (first mSol) ) (= 'dhin (first mSol) ) (= 'dheem (first mSol) )) (cons 'tum (mrMap (rest mSol)))
    (= 'num (first mSol) ) (cons 'ta (mrMap (rest mSol)))
    (list? (first mSol)) (cons '(ta te) (mrMap (rest mSol))) ;;choice between te ta, tum ta and ta te
    :else (cons 'ta (mrMap (rest mSol)))
    )

  )



(defn forced [mr st beat]


  (let [
        forced (apply-rule-map (mrMap mr))
        beat-no (- beat st)
        bar-no (float (/ beat-no 8))
        ]
    forced
    )
  )

(def nome (metronome 236))

;(pattern-play 300 '(ta tum tum (ta te) ta tum tum ta) loud )

;(def mri '(ta tum tum (ta te) ta tum tum tum))
(def loudne '(0.75 0.5 0.5 1 0.5 0.5 1 0.5))

;;loops the metronome at specified tempo, selects a pattern every 8 beats and plays it
(defn looper [st]
  (let [beat (nome)]
    ;(println mr)
    ;(println (metro-tick nome))
    (at (nome beat) (pattern-play (metro-tick nome) mri loudne))

    (cond

    (> (nome) (+ 16 st)) (apply-at (nome (+ 9 beat)) #'looper st [])
    :else (apply-at (nome (+ 9 beat)) #'looper st [])
    )

   )

  )

'((ta tum tum (ta te) (ta te) tum tum ta) (ta tum tum (ta te) (ta te) tum tum tum) (ta tum tum (ta te) ta tum tum ta) (ta tum tum (ta te) ta tum tum tum) (ta tum tum ta (ta te) tum tum ta) (ta tum tum ta (ta te) tum tum tum) (ta tum tum ta ta tum tum ta) (ta tum tum ta ta tum tum tum) (tum tum tum (ta te) (ta te) tum tum ta) (tum tum tum (ta te) (ta te) tum tum tum) (tum tum tum (ta te) ta tum tum ta) (tum tum tum (ta te) ta tum tum tum) (tum tum tum ta (ta te) tum tum ta) (tum tum tum ta (ta te) tum tum tum) (tum tum tum ta ta tum tum ta) (tum tum tum ta ta tum tum tum))


(defn looper1 [st]
  (let [beat (nome)]
    ;(println (kselect mr st bea(stopt))
    ;(println (metro-tick nome))
    (at (nome beat) (pattern-play (metro-tick nome) mri loudness))
    (cond

     (> (nome) (+ 16 st)) (apply-at (nome (+ 32 beat)) #'looper1 st [])
     :else (apply-at (nome (+ 32 beat)) #'looper1 st [])
     )

     )
  )

(defn play-all []

  (let [beat (nome)]

    ;(at (nome (- beat 1)) (play-sample (load-sample "src/accompaniment_system/audio/song.wav") 0.6) )
    ;(looper beat)
    (looper beat)

    )
)
