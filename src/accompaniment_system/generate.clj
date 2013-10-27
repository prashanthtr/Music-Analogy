                                        ;generate combinations

;;substitues correpsonding variable for pause and double taps

(defn doubletap [ mSol Sol pos st subst]

  ( cond

    (empty? Sol) nil
    :else (if ( = -1 (.indexOf pos st) )

            ( cons (first Sol) (doubletap (rest mSol) (rest Sol) pos (+ st 1) subst)  )
            (do (println subst (first mSol) ) ( cons (get subst (first mSol)) (doubletap (rest mSol) (rest Sol) pos (+ st 1) subst)) )
            ;
            )

    )

  )


(defn lengthList [list len]

  (if (empty? list)

    len
    ( lengthList (rest list) (+ len 1))

    )

  )

(defn charAtPos [lists pos st]

  (cond

   (empty? lists) nil
   (= pos st) (first lists)
   :else (charAtPos (rest lists) pos (+ st 1))
   )

  )


(defn doublete [sol1 sol2 sol]

  (if (empty? sol)

    nil
    (cons (cond

     (and (list? sol1) (list? sol2)) sol2

     (and (list? sol1) (not (list? sol2))) (if (and (= sol2 'te) (= (rest sol1) '(te)))
                                             'ta
                                             sol2;(list sol2)
                                             )

     (and (list? sol2) (not (list? sol1))) ( if (and (= sol1 'te) (= (first sol2) 'te))
                                             '(tum ta)
                                             sol2;(list sol2)
                                             )

     (and (not (list? sol2)) (not (list? sol1))) (  if (and (= sol1 'te) (= sol2 'te))
                                                   'ta;(do (println "entered") '(ta) )
                                                   sol2;(do (println "entered") (list sol2) )

                                                   )

     )
          (doublete (first sol) (first (rest sol)) (rest sol)) )

    )

;;; the next hit after double tap can never be the same hit as the ending hit nor on the same fi??

;; current code distinguishes between te and tum as hits on separate finger, but putting them on same finger will reduce the number of possible double hits

;; code that selects the positions to introduce double taps and the double tap to replace, It uses the above simple rules to adjust the subsequent notes in the generated double taps

(defn changesol [sol]

  (cond

   (or (= sol 'te) (= sol 'tum)) 'ta
   :else 'tum

   )

  )

;;three
(defn ruleDouble [sol1 sol2 sol]

  (if (empty? sol)

    nil
    (cons (cond

           (and (list? sol1) (list? sol2)) (if ( = (first sol2) (first (rest sol1)) )
                                             (cons (changesol (first sol2)) (rest sol2))
                                             sol2
                                               )

     (and (list? sol1) (not (list? sol2))) (if (= (first (rest sol1)) sol2)
                                             (changesol sol2)
                                             sol2;(list sol2)
                                             )

     ;;specifically for te only
     (and (list? sol2) (not (list? sol1))) ( if (and (= sol1 'te) (= (first sol2) 'te))
                                              '(tum ta)
                                              sol2;(list sol2)
                                              )

     (and (not (list? sol2)) (not (list? sol1))) (  if (and (= sol1 'te) (= sol2 'te))
             'ta;(do (println "entered") '(ta) )
             sol2;(do (println "entered") (list sol2) )

             )

     )
          (ruleDouble (first sol) (first (rest sol)) (rest sol)) )

    )

  )

;;double hits and pauses
(defn posHit [sol ctr]

  (cond

   (empty? sol) nil
   (list? (first sol)) (cons ctr (posHit (rest sol) (+ ctr 1))
                             )
   (= (first sol) '.)
   (cons ctr (posHit (rest sol) (+ ctr 1))
         )
   :else (posHit (rest sol) (+ ctr 1))
   )

  )

(defn notChangeRule [doublehit accent]

  (cond

   (empty? doublehit) nil
   (not= (.indexOf accent (first doublehit)) -1 ) (notChangeRule (rest doublehit) accent )
   (= (.indexOf accent (first doublehit)) -1 ) (cons (first doublehit) (notChangeRule (rest doublehit) accent ) )

   )

  )

(defn positions [sol]

  (let [hits (posHit sol 0) ]

    (println hits)

    ( println ( map inc hits))

    (cond

     (not= nil hits) (distinct (concat hits (map inc hits)))
     :else nil
     )
    )

  )


;; code that selects the positions to introduce double taps and the double tap to replace, It uses the above simple rules to adjust the subsequent notes in the generated double taps


;; lead plays double taps --
;;positions are selected as those that the the lead plays double taps at and the duration is doubled if it is only for a single note ?

                                        ; forward or backward addition of notes - backward additions as of now

(defn notchangable [accent]

  (if (empty? (rest accent))

    nil
    (cons (first accent) (notchangable (rest accent))  )

    )

  )

(defn nonAccent [pos newsol accent ctr]

  ;(println pos)

  ( if (empty? newsol)

    nil
    (if (and (= -1 ( .indexOf accent ctr)) (= -1 ( .indexOf pos ctr))  )

      (cons ctr (nonAccent pos (rest newsol) accent (+ ctr 1) ) )
      (nonAccent pos (rest newsol) accent (+ ctr 1) )
      )

    )

  )

;(concat pos (nonAccent pos Sol accent 0))
;;identifies the context of the hit in the bar

(defn identify-context [subs ksol st]

  (cond

   ;;do a substitution here, look forward for possible options, apply rule double to check for conflicts


   (empty? ksol) nil
                                        ;pauses by lead
   (not= nil (get subs (charAtPos ksol st 0) )) (get subs (charAtPos ksol st 0)


   ;(cons ) (identify-context subs (rest ksol) (+ st 1))
                                                     )

   ;(= (first ksol) '?p) (cons (get subs '?p) (identify-context subs (rest ksol)) )
   ;double taps by lead
   ;(= (first ksol) '?d) (cons (get subs '?d) (identify-context subs (rest ksol)) )
   :else (charAtPos ksol st 0)
   ;(cons  (identify-context subs (rest ksol) (+ st 1)) )

   )

  )

;; double taps is constraining factor
;; pauses, non accented hits enhance options to improvise

;; double tap in the beginning constraints the rest of the groove
;; double tap is directive of secondary's improvisation as it highlights the points around which variations can be done
;; no double taps offers slightly larger scope for improv

(defn improv-choice [subs ksol st]

  (cond

   (>= st (lengthList ksol 0) ) nil
   (not= nil (get subs (charAtPos ksol st 0)) ) ( cons (identify-context subs ksol st) (improv-choice subs ksol (+ st 1)) )
   :else (cons (charAtPos ksol st 0) (improv-choice subs ksol (+ st 1)) )

   )

  )


(defn resonant [hit]

  (cond

   ( or (= hit 'num) (= hit 'dhin) (= hit 'dheem) (= hit 'cha) (= hit 'thom) (= hit 'dham)  ) true
   :else false
   )

  )

;;checks if non accent and resonant
(defn nonAccentPos [newsol accent ctr]

  ;(println pos)

  ( if (empty? newsol)

    nil
    (if (= -1 ( .indexOf accent ctr))

      (cond
       (true? (resonant (first newsol ))) (cons ctr (nonAccentPos (rest newsol) accent (+ ctr 1) ) )
       :else (nonAccentPos (rest newsol) accent (+ ctr 1) )
       )
      (nonAccentPos (rest newsol) accent (+ ctr 1) )
      )

    )

  )

(defn gen [mSol Sol accent subst improv]

  (let [ pos (notChangeRule (distinct (concat (positions mSol) (nonAccentPos mSol accent 0 ) )) accent) newSol (doubletap mSol Sol pos 0 subst)

 ]
    ; (improv-choice improv (doubletap mSol Sol pos 0 subst) 0)
                                        ;(doubletap mSol Sol pos 0 subst)


    (println "pos" pos "newsol" newSol)
    ;(println (improv-choice improv newSol 0 ))

    ;(println (improv-choice (clojure.set/map-invert subst) newSol 0 ))

    ;(improv-choice (clojure.set/map-invert subst) (doubletap mSol Sol pos 0 subst) )

    ;(do ( println (concat pos (nonAccent pos Sol accent 0)))  )

    ;;changes problem in the diction based on the ohysical constaints
    (cons (first newSol) (ruleDouble (first newSol) (first (rest newSol)) (rest newSol) )
          )

    )
  )


(defn subsequ [pattern st end zero ]

  (if (< zero st )

    ( subsequ (rest pattern) st end (+ zero 1) )
    (if (<= st end)

      ( cons (first pattern) (subsequ (rest pattern) st (- end 1) zero) )
      nil
      )
    )

  )



(defn binary [num]

  (cond

   ; terminating condition
   (zero? num) nil
   :else (cons (mod num 2) (binary (quot num 2)) )

   )

 )

(defn pow [base exp st acc]

  (cond

   (< st exp) (pow base exp (+ st 1) (* acc base))
   :else acc
   )

  )

(defn correctBinary [arr bin]

    ( let [max (lengthList (binary (- (pow 2 (lengthList arr 0) 0 1) 1)) 0)]

      (cond

       (< (lengthList bin 0) max) (correctBinary arr (cons 0 bin))
       :else bin

       )

    )


    )


;;interprets a whole pattern
(defn interpret [comb improvsubst sol st]


  (cond

   (>= st (lengthList comb 0)) nil
   ;substitute from substitution array
   (= (charAtPos comb st 0) 1) (cons (get improvsubst (charAtPos sol st 0)) (interpret comb improvsubst sol (+ st 1)))
   (= (charAtPos comb st 0) 0) (cons (charAtPos sol st 0) (interpret comb improvsubst sol (+ st 1)))
   )

  )


(defn fill [ksol st pos substituted]


  (cond

   (>= st (lengthList ksol 0)) nil
   (not= -1 (.indexOf pos st)) (cons (first substituted ) (fill ksol (+ st 1) pos (rest substituted) ))
   :else (cons (charAtPos ksol st 0) (fill ksol (+ st 1) pos substituted  ))
   )

  )


(defn combination [arr st subst sol ksol pos]

  (cond
   (< st (pow 2 (lengthList arr 0) 0 1)) (do (println (fill ksol 0 pos (interpret (correctBinary arr (reverse (binary st)) ) subst sol 0 ))) (combination arr (+ st 1) subst sol ksol pos) )
   :else nil
   )

  )


(defn notAccent [pos newsol accent ctr]

  ;(println pos)

  ( if (empty? newsol)

    nil
    (if (and (= -1 ( .indexOf accent ctr)) (= -1 ( .indexOf pos ctr))  )

      true
      false
      )

    )

  )



;(not= nil (get subs (first ksol)) ) ( cons (identify subs ksol)  )


;;identify the basic rules for applying different transformations in context
;; get output from current rules, listen to concert recordings - get actual output from data and refine

;; context contains:
;; what did the lead play -- pause, double,
;; what is the previous and the next note -- whether the note is accent/not accent
;; whether the note is base hit or high hit
;; whether note is another pause,
;; whether note is a double hit
;; how far into the past or future am i looking in to make the decision
;; loudness of previous hit, next hits
;; intonation of previous hits , next hits



                                        ;lead plays 3 continuous same hits but in increasing volume,
                                        ;secondary can introduce a double variation?



                                        ;(notchangable (rest accent))
                                        ;(concat pos (nonAccent pos Sol accent  0))

  ;; associative mapping

  ;; it seems that the secondary identifies parts of the lead that are variable and parts of the lead that are constant, once the variable and the constant parts of the lead are identified, then the secondary pattern can be generated as a mixture of variable and constant parts, and the varaible parts can be substituted by different secondary actions namely, double taps, pitch bending, loudness,

  ;; some sections of the pattern are always variable -- like the last beat of the bar,

  ;; The following parts of a lead's groove are come under the variable section that can be varied by the secondary 1) double taps by lead, pauses left by the lead in the groove. It is not yet clear how loudness changes maps to the variable section of the groove. It is also not clear for what all can pitch bending be substituted as n improvisatory choice.

  ;;there seem to be 2 main things that is emerging out this process 1) sections of the lead's groove that are variable 2) choices that the secondary takes to actually vary

  ;; in some cases, it is easy to understand that double taps, pauses in the groove are places that secondary can vary. however, the associative mapping between loudness changes and secondary improvisations is not so clear cut.

  ;;one example of loudness is -- na tha na dhin ta dhin dhin ta is translated to tum tum tum tum tha tum tum ta with increasing empahsis on the first 3 tums. The variable part of the lead occurs when lead plays three consecutively same notes in slightly increasing volume

  ;; when lead plays resonating hits

  ;;loudness -- how does loudness determine the secondary to improvise,

  ;; pitch bending - how does

  ;; and the fourth bar ofcourse


  ;; what are the sections of the lead that secondary varies
  ;; how does he vary
