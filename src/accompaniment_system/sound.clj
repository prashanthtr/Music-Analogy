(load-file "src/accompaniment_system/generate.clj")


(defn ret-sound [sol]

  ;(println sol)
  (let [randomN (rand-int 2) kick (load-sample (freesound-path 2086)) tum (load-sample "src/accompaniment_system/audio/tum.wav" ) ta (load-sample "src/accompaniment_system/audio/ta.wav" ) nam (load-sample "src/accompaniment_system/audio/num.wav" ) dhin (load-sample "src/accompaniment_system/audio/dhin.wav" ) the (load-sample "src/accompaniment_system/audio/thi.wav" ) te (load-sample "src/accompaniment_system/audio/te.wav" ) ]

    (cond

     (= 'tum sol) tum
     (= 'ta sol) ta
     (= 'nam sol) nam
     (= 'dhin sol) dhin
     (= 'the sol) the
     (= 'te sol) te
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


;;stubs for testing
(defn newSol []

(random-subst  '((ta tum tum (ta te) ta tum tum ta)
                   (ta tum (ta te) ta ta tum . ta)
                   ))
  )

(defn soundToRep [sol pos]

  ;(println sol)
  (cond

   (>= pos (- (lengthList sol 0) 1)) nil ;(do (println "term") nil)
   :else (cond

          (and (= '. (charAtPos sol pos 0)) (= '. (charAtPos sol (+ pos 1) 0))) (cons '. (soundToRep sol (+ pos 2)))
          (and  (not= '. (charAtPos sol pos 0)) (= '. (charAtPos sol (+ pos 1) 0))) (cons (charAtPos sol pos 0) (soundToRep sol (+ pos 2)))
          (and (not= '. (charAtPos sol pos 0)) (not= '. (charAtPos sol (+ pos 1) 0))) (cons (list (charAtPos sol pos 0) (charAtPos sol (+ pos 1) 0)) (soundToRep sol (+ pos 2) ))
          )
   )

  )

(defn repToSound [sol]

  ;(println sol)
  (cond
   (= nil (first sol)) nil
   (empty? sol) nil
   (not (list? (first sol))) (concat (list (first sol) '.) (repToSound (rest sol)))
   :else (concat (list (first (first sol)) (first (rest (first sol)))) (repToSound (rest sol)))
   )

  )


(def metrono (metronome 200))
(def mridangam '(nam the dhin dhin . dhin dhin (nam the)))
(def volum '(0.9 0.3 0.3 0.3 0.5 0.3 1 0.3 0.25 0.3 0.5 0.3 1 0.2 0.5 0.5))
(def vol-system4 '(0.9 0.3 0.5 1 0.25 0.5 1 0.5))

;(looper metrono (repToSound mridangam) (list mridangam) volum 0 0 )

;(looper metrono (repToSound (mriMap mridangam)) (systemv1 mridangam) volum 0 0 )

;(looper metrono (repToSound (mriMap mridangam)) (systemv2 mridangam) volum 0 0 )

;(looper metrono (repToSound (mriMap mridangam)) (systemv3 mridangam) volum 0 0 )

;;tum te hangs program

(defn ret-seq []

  '(tum (ta te) tum ta ta tum tum ta)

  )

(defn list2Vec [list]

  (into [] list)

  )

(defn time-hit [tempo sol]

  (cond

   (empty? sol) nil
   (list? (first sol)) (concat (list (/ tempo 2) (/ tempo 2)) (time-hit tempo (rest sol) )  )
   :else (cons tempo (time-hit tempo (rest sol) ) )
   )

  )

(defn d2Sr [sol]

  (cond

   (empty? sol) nil
   (list? (first sol)) (concat (list (first (first sol)) (first (rest (first sol)))) (d2Sr (rest sol) )  )
   :else (cons (first sol) (d2Sr (rest sol) ) )
   )

  )


(defn tempo-s [tempo]

  (*  (/ 60 tempo) 1000)

  )

(defn pat-vol [pat vol]

  (cond

   (empty? pat) nil
   (list? (first pat)) (concat (list (first vol) (first vol)) (pat-vol (rest pat) (rest vol)) )
   :else (cons (first vol) (pat-vol (rest pat) (rest vol)))

   )

  )



(defn create-schedule [speed tempo st ctr]

  (cond

   (> st (lengthList speed 0)) nil
   (= st 0) (cons ctr (create-schedule speed tempo (+ st 1) ctr))
   :else (cons (+ ctr (charAtPos speed (- st 1) 0)) (create-schedule speed tempo (+ st 1) (+ ctr (charAtPos speed (- st 1) 0)))  )
   )

  )

;;interprets the hits on the pattern based as single or double and plays them
(defn pattern-play [tempo pat forced-vol]

  (let [vol (pat-vol pat forced-vol) tmp pat speed (list2Vec (time-hit tempo tmp)) schedule (create-schedule speed tempo 0 0) pattern (list2Vec (d2Sr tmp))  ]

    ;(println pattern)

    (dorun (map-indexed (fn [i n] (at (+ (now) (nth schedule i)) (play-sample (ret-sound n) (nth vol i)))) pattern))
    )

  )


(defn ret-mr []

  '(nam dhin dhin (nam ta) nam dhin dhin nam)

  )

(defn mr-play [tempo volume]

  (let [vol (list2Vec volume) tmp (ret-mr) speed (list2Vec (time-hit tempo tmp)) schedule (create-schedule speed tempo 0 0) pattern (list2Vec (d2Sr tmp)) ]

    (dorun (map-indexed (fn [i n] (at (+ (now) (nth schedule i)) (play-sample (ret-sound n) (nth vol i)))) pattern))

    )

  )

(def loud '(0.9 0.8 0.8 0.9 0.9 0.8 0.8 0.9))
;(pattern-play 300 '(nam dhin dhin (nam ta) nam dhin dhin nam) loud )
;(pattern-play 300 '(ta tum tum (ta te) ta tum tum ta) loud )

(def mr '(nam dhin dhin (nam thi) nam dhin dhin nam))
(def kan '(ta tum tum (ta te) ta tum tum ta))
(def nome (metronome 200))


(defn kselect [mr]

  (let [
        forced (mriMap mr)
        discr (main mr '((0 4)) {'. '?p '(ta te) '?d '(te ta) '?d '(ta tum) '?d '(tum tum) '?d '(tum ta) '?d  'tum '?nA 'ta '?nA 'te '?nA}  )
        list (cons forced discr)
        random (random-subst list 0)

        ]
    ;(println random)
    (println random)
    random
    )

  )

;;loops the metronome at specified tempo, selects a pattern every 8 beats and plays it
(defn looper [mr st]
  (let [beat (nome)]
    ;(println mr)
    ;(println (metro-tick nome))
    (at (nome beat) (pattern-play (metro-tick nome) mr loud))

    (cond

    (> (nome) (+ 32 st)) nil
     :else (apply-at (nome (+ 8 beat)) #'looper mr st []))

   )

  )

(defn looper1 [mr st]
  (let [beat (nome)]
    ;(println (kselect mr))
    ;(println (metro-tick nome))
    (at (nome beat) (pattern-play (metro-tick nome) (kselect mr) loud))
    (cond

     (> (nome) (+ 32 st)) nil
     :else (apply-at (nome (+ 8 beat)) #'looper1 mr st []))

     )
  )

;(looper (metronome 200))
;(looper1 (metronome 200))


(defn mridangam [mSol]

  (let [beat (nome)]

    (looper mSol beat)
    (looper1 mSol beat)

    )


  )


(defn record []
  (println "User Input" (nome))
  )


(on-event [:midi :note-on]
          (fn [e]
            (let [note (:note e)
                  vel  (:velocity e)
                  time (:timeStamp e)
                  ]
              (record)
              ))
          ::keyboard-handler)

;(mridangam '(nam dhin dhin (nam ta) nam dhin dhin nam))
