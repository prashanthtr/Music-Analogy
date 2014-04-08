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

(defn time-hit [tempo sol]

  (cond

   (empty? sol) nil
   (list? (first sol)) (concat (list (/ tempo 2) (/ tempo 2)) (time-hit tempo (rest sol) )  )
   :else (cons tempo (time-hit tempo (rest sol) ) )
   )

  )

(defn create-schedule [speed tempo st ctr]

  (cond

   (> st (lengthList speed 0)) nil
   (= st 0) (cons ctr (create-schedule speed tempo (+ st 1) ctr))
   :else (cons (+ ctr (charAtPos speed (- st 1) 0)) (create-schedule speed tempo (+ st 1) (+ ctr (charAtPos speed (- st 1) 0)))  )
   )
  )

(defn oneDvec [list]

  (reduce
   concat
   (map

    (fn [d]

      (cond

       (list? d) d
       :else (take 1 (create-arr d))
       )
      )
    list
    )
   )
  )


;[ta tum tum (ta ta)]
(defn scale-dur [list]

  (reduce
   concat
   (map

    (fn [d]

      (cond
       (list? d) (take (lengthList d 0) (create-arr (/ 1 (lengthList d 0))) )
       :else '(1)
       )

      )
    list)
   )
  )


;;interprets the hits on the pattern based as single or double and plays them
(defn pattern-play [tempo diction]

  (let [template (into [] (scale-dur diction))
        diction (into [] (oneDvec diction))
        speed (into [] (map (partial * tempo) template))
        schedule (create-schedule speed tempo 0 0)
        ]
    ;(println "template" template "diction" diction "speed" speed)
    (dorun (map-indexed (fn [i n] (at (+ (now) (nth schedule i)) (play-sample (ret-sound n) 1))) diction))
    )

  )


(def nome (metronome 150))

;; needs to be cleaned

(defn looper1 [st structure pos]
  (let [beat (nome)
        l1 (get structure 'l1)
        l2 (get structure 'l2)
        a1 (get (get structure 'a1) 'noteDuration)
        choices (cond
                 (= nil a1) (list (get (get structure 'a1) 'diction))
                 :else (let [
                             a2 (get structure 'a2)
                             ]
                         (cond
                          (= nil a2) (list (get (get structure 'a1) 'diction))
                          :else a2
                          )
                         )
                 )
        diction (nth choices pos)
        ]
    (println "a2" diction)
    (at (nome beat) (pattern-play 200 (concat (get (get structure 'a1) 'diction) diction )  ))

    (draw-l1-l2 l1 l2 'L1 'L2)
    (draw-l1-l2 (rand-nth (descriptions (get (get structure 'a1) 'diction))) (rand-nth (descriptions diction)) 'A1 'A2 )

    (apply-at (nome (+ 6 beat)) #'looper1 (nome) structure (mod (inc pos) (lengthList choices 0)) [])
     )

  )

(defn looper [st diction]

  (let [
        beat (nome)
        ]
    (at (nome beat) (pattern-play 200 diction ))
    (apply-at (nome (+ 6 beat)) #'looper (nome) diction [])
    )

  )

(defn play [l1 l2 a1]

  (let [
        p1 (main l1 l2 a1)
        beat (nome)
        lead (concat l1 l2)
        ]
    (looper beat lead)
    (looper1 (+ beat 16) p1 0)

    )

  )


(defn fun [a c d]

  (fn l [c d]

    (cond
     (= c 0) 1
     :else (do     (println (+ a d))
                   (l (dec c) d)
                   )
     )

    )

  )


loop

pattern-play
draw-tree
