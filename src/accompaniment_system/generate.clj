;This program uses multiple rules to generate different possible improvisations. It does it by first identifying the parts of the lead that can be improvised and substitutes the appropriate improvisatory action for those parts.

;;substitues corresponding "variable" for pause, double taps and accented or non accented positions, based on the "subst" mapping

(defn doubletap [ mSol Sol pos st subst]

  ( cond

    (empty? Sol) nil
    :else (if ( = -1 (.indexOf pos st) )

            ( cons (first Sol) (doubletap (rest mSol) (rest Sol) pos (+ st 1) subst)  )
            ( cons (get subst (first mSol)) (doubletap (rest mSol) (rest Sol) pos (+ st 1) subst))
                                        ;(do (println subst (first mSol) )  )
            ;
            )

    )

  )

;returns lenght of a list

(defn lengthList [list len]

  (if (empty? list)

    len
    ( lengthList (rest list) (+ len 1))

    )

  )

;returns character at a particular position in the list
(defn charAtPos [lists pos st]

  (cond

   (empty? lists) nil
   (= pos st) (first lists)
   :else (charAtPos (rest lists) pos (+ st 1))
   )

  )


;; code that selects the positions to introduce double taps and the double tap to replace, It uses the above simple rules to adjust the subsequent notes in the generated double taps



;;changes the hit in the case of continuous repetition of the same hit even after double tap. current code distinguishes between te and tum as hits on separate finger, but putting them on same finger will reduce the number of possible double hits

(defn changesol [sol]

  (cond

   (or (= sol 'te) (= sol 'tum)) 'ta
   :else 'tum

   )

  )





;;;Physical constraints


;;This constraint takes care of multiple ta ta's or te te's within a double hit
(defn doublete [sol]


  (cond

   (empty? sol) nil
   (or (= (first sol) '(ta ta)) (= (first sol) '(te te))) (cons (cons (changesol (first (first sol))) (rest (first sol))) (doublete (rest sol)))
   :else (cons (first sol) (doublete (rest sol)))

   )

  )

;;double hit == more generalized version of double te, knows only sol in the context
(defn doubleteta [sol]


  (cond

   (empty? sol) nil
   (or (= sol '(ta ta)) (= sol '(te te))) (cons (changesol (first sol)) (rest sol))
   :else sol

   )

  )

;;Derived from Rule1: The hit following a double tap cannot be the same hit as the ending hit of the tap.

;; physical constraints within different hits in the pattern
;; does not distinguishes between te and tum as hits on separate fingers

(defn finger [sol]

  (cond

   (or (= sol 'te) (= sol 'tum)) 1
   :else 2
   )

  )

(defn ruleDouble [sol1 sol2 sol]

  (if (empty? sol)

    nil
    (cons (cond

           (and (list? sol1) (list? sol2)) (if ( = (finger (first sol2)) (finger (first (rest sol1))) )
                                             (cons (changesol (first sol2)) (rest sol2))
                                             sol2
                                               )

     (and (list? sol1) (not (list? sol2))) (if (= (finger (first (rest sol1))) (finger sol2))
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


;;identifies the position of double hits and pauses in 8 beat groove
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

;; identifies the unchangable or forced hits in a 8 beat groove based on the accent position

(defn notChangeRule [doublehit accent]

  (cond

   (empty? doublehit) nil
   (not= (.indexOf accent (first doublehit)) -1 ) (notChangeRule (rest doublehit) accent )
   :else (cons (first doublehit) (notChangeRule (rest doublehit) accent ) )

   )

  )

;; identifies the positions of the double hits, pauses from mridangam sol and returns a list with next positions of the hits as "Variable" too.

(defn positions [sol]

  (let [hits (posHit sol 0) ]

                                        ;(println hits)
                                        ;( println ( map inc hits))

    (cond

     (not= nil hits) hits ;(distinct (concat hits (map inc hits)))
     :else nil
     )
    )

  )


;; code that selects the positions to introduce double taps and the double tap to replace, It uses the above simple rules to adjust the subsequent notes in the generated double taps


;; lead plays double taps --
;;positions are selected as those that the the lead plays double taps at and the duration is doubled if it is only for a single note ?

                                        ; forward or backward addition of notes - backward additions as of now


;;returns not accented positions in a groove
                                        ;(concat pos (nonAccent pos Sol accent 0))

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


;;identifies the context of the variable hit in the bar based on the whether the variability is a result of pause, double hit or non accented hit. This is the decision making part of the whole code:

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

;;check whether a hit is a resonant hit

(defn resonant [hit]

  (cond

   ( or (= hit 'num) (= hit 'dhin) (= hit 'dheem) (= hit 'cha) (= hit 'thom) (= hit 'dham) (= hit 'tum)  ) true
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
       :else (cons ctr (nonAccentPos (rest newsol) accent (+ ctr 1) ))
       )
      ;(cons ctr (nonAccentPos (rest newsol) accent (+ ctr 1) ) )
       (nonAccentPos (rest newsol) accent (+ ctr 1) )
       )

      )

  )

;;returns a subsequence of an list given start and end position

(defn subsequ [pattern st end zero ]

  (if (< zero st )

    ( subsequ (rest pattern) st end (+ zero 1) )
    (if (<= st end)

      ( cons (first pattern) (subsequ (rest pattern) st (- end 1) zero) )
      nil
      )
    )

  )


;;returns a binary sequence appended with appropriate 0's given a number. used in generating combinations.

(defn binary [num]

  (cond

   ; terminating condition
   (zero? num) nil
   :else (cons (mod num 2) (binary (quot num 2)) )

   )

  )

;;apower given a base

(defn apow [base exp st acc]

  (cond

   (< st exp) (apow base exp (+ st 1) (* acc base))
   :else acc
   )

  )

;;corrects the binary conversion by appending zeroes in front
(defn correctBinary [arr bin]

    ( let [max (lengthList (binary (- (apow 2 (lengthList arr 0) 0 1) 1)) 0)]

      (cond

       (< (lengthList bin 0) max) (correctBinary arr (cons 0 bin))
       :else bin

       )

    )

    )

;; Turns the combinations array  in binary representation to variables and their substitutions. 1's in the representation are substituted and zeroes are left as variable.

(defn interpret [comb improvsubst sol st]


  (cond

   (>= st (lengthList comb 0)) nil
   ;substitute from substitution array
   (= (charAtPos comb st 0) 1) (cons (get improvsubst (charAtPos sol st 0)) (interpret comb improvsubst sol (+ st 1)))
   (= (charAtPos comb st 0) 0) (cons (charAtPos sol st 0) (interpret comb improvsubst sol (+ st 1)))
   )

  )



(defn variable [var]

  (cond

   (= var '?p) true
   (= var '?nA) true
   (= var '?d) true
   :else nil

   )
  )



(defn pos-variation [sol st]

  (cond

   (= true (variable (charAtPos sol st 0))) (pos-variation sol (+ st 1) )
   :else (- st 1)

   )

  )


(defn group-variations [sol st]

  (let [start st]

    (cond

     (>= st (lengthList sol 0)) nil
     (variable (charAtPos sol start 0) )
     ;;consecutive
     (let [end (pos-variation sol (+ start 1)) dif (- end start) start end]

       (cons (cond

              (= (charAtPos sol (- end dif) 0) '?p) '?p
              :else '?nA

              )

             (group-variations sol (+ start 1)))

       )
     :else (cons (charAtPos sol start 0) (group-variations sol (+ start 1)))
   )

    )
)


; Fills the kanjira sol with substituted array
(defn fill [ksol st pos substituted]


  ;(println substituted)
  (cond

   (>= st (lengthList ksol 0)) nil
   (not= -1 (.indexOf pos st)) (cond

                                (= nil (variable (first substituted))) (cons (first substituted ) (fill ksol (+ st 1) pos (rest substituted) ))
                                :else (cons (charAtPos ksol st 0) (fill ksol (+ st 1) pos (rest substituted)  ))

                                )

   :else (cons (charAtPos ksol st 0) (fill ksol (+ st 1) pos substituted  ))
   )

  )


;;(if (variable (first substituted) )

                                        ;  (cons (cons (charAtPos ksol st 0) (list (first substituted))) (fill ksol (+ st 1) pos substituted  ))

  ;)

;;generates 2^n combinations based on the position array of pauses, double hits and non accented resonant positions. Also needs, original kanjira sol, substituions and position array in the context.

(defn combination [st subst sol ksol pos]

  (cond
   (< st (apow 2 (lengthList pos 0) 0 1))

   (let [ newSol (doublete (fill ksol 0 pos (interpret (correctBinary pos (reverse (binary st)) ) subst sol 0 )) ) ]

    ; (println st (cons (first newSol) (ruleDouble (first newSol) (first (rest newSol)) (rest newSol)) ))

     (cons (cons (first newSol) (ruleDouble (first newSol) (first (rest newSol)) (rest newSol)) ) (combination (+ st 1) subst sol ksol pos) )

     ;(combination (+ st 1) subst sol ksol pos)

     )
;   (do (println (doublete (fill ksol 0 pos (interpret (correctBinary pos (reverse (binary st)) ) subst sol 0 )) )) (combination (+ st 1) subst sol ksol pos) )
   :else nil
   )

  )

(defn variables [sol subst]

  (cond

   (empty? sol) nil
   (not= nil (get subst (first sol))) (cons (first sol) (variables (rest sol) subst) )
   :else (variables (rest sol) subst)

   )


  )

;;no empty or 3 substitutions

(defn gen-subst [sol subst pos ksol]

  (cond

   (empty? (first subst)) nil
   ( or (= 3 ( lengthList (first (first subst)) 0)) (= 3 ( lengthList (rest (first subst)) 0))  ) (gen-subst sol (rest subst) pos ksol)


;(empty? (first (first subst))) (empty? (rest (first subst)) )

   :else
   (let [improv {'?p (first (first subst)) '?nA (rest (first subst)) '?d '(ta te)}]

     ;(do (println (combination 0 improv (variables sol improv) ksol pos))   )

     (concat (combination 0 improv (variables sol improv) ksol pos) ( gen-subst sol (rest subst) pos ksol))

     ;(let [ concated (distinct (concat (distinct (combination 0 improv (variables sol improv) ksol pos)) (gen-subst sol (rest subst) pos ksol)))]

                                        ;(lengthList concated 0)

       ;)


     )
   )

  )


;;returns a list of same element as map
(defn samemap [perm]

  (cond

  (empty? perm) nil
  :else ( cons (list (first perm) (first perm) ) (samemap (rest perm) )  )

   )

  )

;; returns single and double substitutions possible -- including () no substitution
(defn apowerset [perm-sol]

  (let [substitutions  (concat '(()) (combinations perm-sol 1) (concat (combinations perm-sol 2)  (map reverse (combinations perm-sol 2)) (samemap perm-sol) )
                               )]

    (distinct ( concat (first substitutions) (map doubleteta (rest substitutions)) ))

    )

  )


;;neatly displays

(defn display [list]

  (cond

   (empty? list) nil
   :else (do (println (first list)) (display (rest list)))

   )

  )

;;finds positions from newsolthat contains '?p, '?nA etc
(defn findpos [sol st]

  (cond

   (empty? sol) nil
   (variable (first sol)) (cons st (findpos (rest sol) (+ st 1)))
   :else (findpos (rest sol) (+ st 1))
   )

  )


;;test case  (gen '(num dhin dhin dhin num dhin . dhin) '(ta tum tum ta ta tum tum ta) '(0 1 2 3 4 5 7) {'. '?p '(num tha) '?d 'num '?nA 'dhin '?nA} )

;;generates a seconary response to lead based on a given lead, secondary pattern, accent structure, substition variables and possible improvisational substitions.

(defn gen [mSol Sol accent subst]

  (let [ pos (sort (notChangeRule (distinct (concat (positions mSol) (nonAccentPos mSol accent 0 ) )) accent)) newSol (doubletap mSol Sol pos 0 subst) cartprod (cartesian-product (apowerset '(ta te tum)) (apowerset '(ta te tum)))
        ;(cartesian-product (subsets '(ta te tum)) (subsets '(ta te tum)) )
        ]
    ; (improv-choice improv (doubletap mSol Sol pos 0 subst) 0)
                                        ;(doubletap mSol Sol pos 0 subst)

    (println "Substitutions recognized" subst)
    (println "Variable positions" pos )
    (println "Varialbe groove" newSol)
    (print "Variations are:")

    (let [ newSol2 (group-variations newSol 0) pos2 (findpos newSol2 0) accomp (gen-subst newSol cartprod pos Sol)]


      (display (distinct accomp) )
      (println "Total Variations" (lengthList accomp 0) )
      (println "Non redundant variations" (lengthList (distinct accomp) 0))

      )


    ;(println (lengthList (distinct (concat (distinct (combination 0 improv (variables newSol improv) Sol pos)) (distinct (combination 0 improv2 (variables newSol improv2) Sol pos)) (distinct (combination 0 improv3 (variables newSol improv3) Sol pos))  ))  0)

             ;)

                                        ;(println (improv-choice improv newSol 0 ))

                                        ;(println (improv-choice (clojure.set/map-invert subst) newSol 0 ))

                                        ;(improv-choice (clojure.set/map-invert subst) (doubletap mSol Sol pos 0 subst) )

                                        ;(do ( println (concat pos (nonAccent pos Sol accent 0)))  )

    ;;changes problem in the diction based on the ohysical constaints

   ;(cons (first newSol) (ruleDouble (first newSol) (first (rest newSol)) (rest newSol) )
          ; )

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


(defn mriMap [mSol]

  (if (empty? mSol)

    nil
    (cons


     (cond

      (empty? mSol) nil
      (= '. (first mSol)) '.
      (or (= 'num (first mSol) ) (= 'dham (first mSol) ) (= 'dhin (first mSol) ) (= 'dheem (first mSol) )) 'tum
      (list? (first mSol)) '(ta te) ;;choice between te ta, tum ta and ta te
      :else 'ta
      )

     (mriMap (rest mSol))
     )

    )


  )

;tum followed by te is changed to tum
; te followed by tum is replaced by ta

(defn tumteRule [ksol st]

  (cond

   (>= st (lengthList ksol 0)) nil
   ( and (= 'te (charAtPos ksol st 0) ) (= 'tum (charAtPos ksol (- st 1) 0) ) ) (cons 'tum (tumteRule ksol (+ st 1)) )
   ( and (= 'te (charAtPos ksol st 0) ) (= 'tum (charAtPos ksol (+ st 1) 0) ) ) (cons 'ta (tumteRule ksol (+ st 1)) )
   :else (cons (charAtPos ksol st 0) (tumteRule ksol (+ st 1)) )
   )

  )


(defn random-subst [list pos]

  ( charAtPos list (rand-int (lengthList list 0 )) 0 )

  )


;; ta te followed by pause is awkard, hence is usually played with a tum
(defn tatetumRule [ksol st]

  (cond

   (>= st (lengthList ksol 0)) nil
   ( and (= '. (charAtPos ksol st 0) ) (= '(ta te) (charAtPos ksol (- st 1) 0) ) ) (cons (random-subst '(tum ta) 0) (tatetumRule ksol (+ st 1)) )
   :else (cons (charAtPos ksol st 0) (tatetumRule ksol (+ st 1)) )
   )


  )

;;applies the physical constraints on a sol
(defn apply-rule-map [ksol]

  (tatetumRule (tumteRule (doubleteta (cons (first ksol) (ruleDouble (first ksol) (first (rest ksol)) (rest ksol) ))) 0) 0)

  )


(defn single-level-subst [hit pos]

  (cond

   (= pos 0) (random-subst '(tum ta te (ta te)) pos) ;;first position
   (list? hit) (random-subst '(tum (ta te) .) pos)
   (resonant hit) (random-subst '(tum (ta te) .) pos)
   :else (random-subst '(ta te (ta te) .) pos)
   )

  )

(defn double-level-subst [pos]

  (cond

   (not= 0 pos) (random-subst '(((ta te) tum) ((ta te) (ta te)) (tum .)) pos)
   :else (random-subst '((tum (ta te) ) (tum tum) ((ta te) (ta te)) (tum .)) pos)

   )

  )

(defn third-level-subst [pos]

  (cond

   (not= 0 pos) (random-subst '(((ta te) (ta te) (ta te))  ((te ta) (te ta) (te ta)) ) pos)
   :else (random-subst '((tum tum tum) (tum ta tum) ((te ta) (te ta) (te ta)) ((ta te) (ta te) (ta te))) pos)

   )

  )


(defn ret-op [var]


  (cond

   (list? var) concat ;(do (println var "concat") concat)
   :else cons ;(do (println var "cons") cons)
   )

  )


;;makes substitution taking into account the speed of the hits in the substitution
(defn multi-level-subst [sol st]

                                        ; single subst
                                        ;double subst
                                        ;triple subst

  (if (>= st (lengthList sol 0))

    nil
    (let [hit (charAtPos sol st 0)]

      (cond
   ;;double


  ; (and (list? (charAtPos sol st 0)) (list? (charAtPos sol (+ st 1) 0)) (list? (charAtPos sol (+ st 2) 0)) )
   ;(do (println "triple subs") (cons (third-level-subst st) (multi-level-subst sol (+ st 3) )))

   ;(and (list? (charAtPos sol st 0)) (list? (charAtPos sol (+ st 1) 0)) )
   ;(do (println "Double subs") (cons (double-level-subst st) (multi-level-subst sol (+ st 2) )))

       (>= st (lengthList sol 0)) nil
       (and (variable hit ) (variable (charAtPos sol (+ st 1) 0)) (variable (charAtPos sol (+ st 2) 0)) )
       (let [third-sub (third-level-subst st)]

         ;(println third-sub "triple subs")
         ((ret-op third-sub ) third-sub (multi-level-subst sol (+ st 3) ))

         )

       (and (variable hit ) (variable (charAtPos sol (+ st 1) 0)) )
       (let [second-sub (double-level-subst st)]

         ;(println second-sub "double subs")
         ((ret-op second-sub ) second-sub (multi-level-subst sol (+ st 2) ))

         )

       (variable hit)
       (let [first-sub (single-level-subst hit st)]

         ;(println first-sub "single subs")
         (cons first-sub (multi-level-subst sol (+ st 1) ))

         )

       :else ( cons hit (multi-level-subst sol (+ st 1) ))
       )

      )

    )


  )

(defn bar-improv [sol ctr]


  ;(println sol)
  (cond

   (>= ctr 4) nil
   :else (bar-improv (multi-level-subst sol 0) (+ ctr 1))
   )


  )

(defn var-sub [ Sol pos st subst]

  ( cond

    (>= st (lengthList Sol 0)) nil
    (= -1 (.indexOf pos st)) (cons (charAtPos Sol st 0) (var-sub Sol pos (+ st 1) subst))
    :else  (cons (get subst (charAtPos Sol st 0)) (var-sub Sol pos (+ st 1) subst))
    )

  )

(defn gen-subsumption [Sol accent subst st]


  (let [ pos (sort (notChangeRule (distinct (concat (positions Sol) (nonAccentPos Sol accent 0 ) )) accent) ) newSol (var-sub (apply-rule-map Sol) pos 0 subst)
        ]

    ;(notChangeRule (distinct (concat (positions Sol) (nonAccentPos Sol accent 0 ) )) accent)
    ;(println "pattern" newSol)

    (cond

       (>= st 3) nil
       :else
       (let [substSol (multi-level-subst newSol 0)]
         ;(println "variation" substSol)
         ;(distinct )
         (cons substSol (gen-subsumption substSol accent subst (+ st 1)))
         )
       )
    )
  )

;(main '(num dhin . dhin num dhin . dhin) '(0 1 2 3 4 5 7) {'. '?p '(ta te) '?d '(te ta) '?d '(ta tum) '?d '(tum tum) '?d '(tum ta) '?d  'tum '?nA 'ta '?nA 'te '?nA} )

(defn main [mSol comb subst]

  (cond

   (empty? comb) nil
   :else (let [accent (first comb) pos (sort (notChangeRule (distinct (concat (positions mSol) (nonAccentPos mSol accent 0 ) )) accent) ) newSol (mriMap mSol)
                                        ;(var-sub (apply-rule-map (mriMap mSol)) pos 0 subst)
               ;(notChangeRule (distinct (concat (positions mSol) (nonAccentPos mSol accent 0 ) )) accent)
               ]
                                        ;(println "main" pos newSol)
           (let [ output (map apply-rule-map (concat (gen-subsumption newSol accent subst 0) (main mSol (rest comb) subst)))]

             ;(display (distinct output))
             output
             ;(println "length" (lengthList output 0))

             )

           )
   )

  )


(defn collectingData [mSol comb subst st]


  (cond

   (>= st 20) nil
   :else (distinct (concat (main mSol comb subst) (collectingData mSol comb subst (+ st 1))  )  )
   )

  )


;; TA followed BY A PAUSE? - tum very low loudness(high loudness) (forced choice -> 0 low else noatural loudness)
;; douuble ta does not come in the end of the bar, prev ta replaced by tum or .




;; multiple points of variation in the groove
;; but ideally limit to 1 variation in a bar
;; certain sequence of variations imply a particlar build up whereas certain others are random
;; substitutions take place at multiple levels -->

;; first level -> each pause or either accented / non accented hit is substituted


;; second level -> 2 hits
;;1. ta te ta te -> tum tum or tum ta te (not much pitch bending) and vice versa,

;; third level -> 3 hits
;obvious examples -> ta te ta te ta te -> tum tum tum or tum ta tum with pitch bending, tum tum tum with increased loudness ( vice versa cases)

;; for the sake of this research, max third level for now.

;; for each substitution, there are double and triple levels possible

;;context is identified as whether it is a pause, single hit( accented, non accented) or double hit


;; non accented could stay as a non accented hit in the single substitution, in which case an adjacent pause is not added to it, however, if the non accented hit is replaced by a double hit , then a pause/nA/accented could combine with it in the second level of substitution and form a second variation to which another nA hit could add on.

;; it is like a chain reaction, the first intrusion into the lead's pattern is the tricky part. after that, it leads to a chain of merging and changing and substituion by repeatedly adding on nearby hits to the substitutions and finally building up to conclude the improv cycle.


;; typesetting to check at the same position different possiblities
;; check single substitutions at different positions
;; check double, triple substitutions at different positions
;; at the moment excel ?


;; tum at the end is always followed by a pause
;; or else tum is replaced by a ta

; tum followed by pause , followed by a "ta" or "te" - no generalization




;(main '(num the dhin dhin the dhin dhin the) '((1 2 3 4 5 6 7)) {'. '?p '(ta te) '?d '(te ta) '?d '(ta tum) '?d '(tum tum) '?d '(tum ta) '?d  'tum '?nA 'ta '?nA 'te '?nA} )
                                        ;

                                        ;1

(def variations '(((ta te) ta tum tum ta tum tum ta)
(tum ta tum tum ta tum tum ta)
(te ta tum tum ta tum tum ta)
(ta ta tum tum ta tum tum ta)

;1 and 2

(tum tum tum tum ta tum tum ta)
(tum . tum tum ta tum tum ta)
(tum (ta te) ta tum ta tum tum ta)
((ta te) (ta te) ta tum ta tum tum ta)
(tum . ta tum ta tum tum ta)
(tum tum ta tum ta tum tum ta)

;1 and 3

(ta ta ta tum ta tum tum ta)
(tum ta ta tum ta tum tum ta)
(te ta ta tum ta tum tum ta)
(te ta (ta te) ta ta tum tum ta)
(te ta te ta ta tum tum ta)
(ta ta ta ta ta tum tum ta)
(tum ta (ta te) ta ta tum tum ta)
((ta te) ta (ta te) ta ta tum tum ta)
(tum ta te ta ta tum tum ta)
(ta ta te ta ta tum tum ta)
(te ta ta ta ta tum tum ta)
((ta te) ta te ta ta tum tum ta)
(tum ta ta ta ta tum tum ta)
((ta te) ta ta ta ta tum tum ta)


;1 and 4


(te ta tum tum ta tum tum ta)
((ta te) ta tum (ta te) ta tum tum ta)
(te ta tum ta ta tum tum ta)
(te ta tum (ta te) ta tum tum ta)
(tum ta tum ta ta tum tum ta)
(ta ta tum tum ta tum tum ta)
(ta ta tum ta ta tum tum ta)
(tum ta tum (ta te) ta tum tum ta)
((ta te) ta tum tum ta tum tum ta)
(ta ta tum (ta te) ta tum tum ta)
((ta te) ta tum ta ta tum tum ta)


;1 and 5


(tum ta tum tum (ta te) ta tum ta)
(ta ta tum tum (ta te) ta tum ta)
(tum ta tum tum tum ta tum ta)
(tum ta tum tum ta ta tum ta)
(te ta tum tum tum ta tum ta)
(ta ta tum tum tum ta tum ta)
((ta te) ta tum tum (ta te) ta tum ta)
(te ta tum tum ta ta tum ta)
((ta te) ta tum tum ta ta tum ta)
((ta te) ta tum tum tum ta tum ta)
(te ta tum tum (ta te) ta tum ta)
(ta ta tum tum ta ta tum ta)

;1 and 6


((ta te) ta tum tum ta (ta te) ta ta)
(tum ta tum tum ta te ta ta)
((ta te) ta tum tum ta te ta ta)
(ta ta tum tum ta ta ta ta)
(tum ta tum tum ta ta ta ta)
(ta ta tum tum ta (ta te) ta ta)
(te ta tum tum ta ta ta ta)
((ta te) ta tum tum ta ta ta ta)
(tum ta tum tum ta (ta te) ta ta)
(te ta tum tum ta te ta ta)
(ta ta tum tum ta te ta ta)

;1 and 7


(te ta tum tum ta tum ta ta)
((ta te) ta tum tum ta tum tum ta)
(tum ta tum tum ta tum (ta te) ta)
(ta ta tum tum ta tum ta ta)
(tum ta tum tum ta tum ta ta)
((ta te) ta tum tum ta tum (ta te) ta)
(te ta tum tum ta tum tum ta)
(ta ta tum tum ta tum tum ta)
(ta ta tum tum ta tum (ta te) ta)
((ta te) ta tum tum ta tum ta ta)
(tum ta tum tum ta tum tum ta)


;1 and 8

((ta te) ta tum tum ta tum tum (ta te))
(tum ta tum tum ta tum tum (ta te))
(tum ta tum tum ta tum tum ta)
(ta ta tum tum ta tum tum (ta te))
(te ta tum tum ta tum tum tum)
((ta te) ta tum tum ta tum tum tum)
(tum ta tum tum ta tum tum tum)
((ta te) ta tum tum ta tum tum ta)
(ta ta tum tum ta tum tum ta)
(te ta tum tum ta tum tum ta)
(ta ta tum tum ta tum tum tum)


;1, 2 and 3


(tum tum tum tum ta tum tum ta)
((ta te) (ta te) (ta te) ta ta tum tum ta)
((te ta) (te ta) (te ta) tum ta tum tum ta)
(tum ta tum ta ta tum tum ta)
(tum tum tum ta ta tum tum ta)
(tum ta tum tum ta tum tum ta)
 )
)




(defn subst-pos [subst sol pos]

  (reverse (into () (assoc (into [] sol ) pos subst)))

  )

(defn subst-pos-dbl [subst sol pos]

  (reverse (into () (assoc (into [] sol ) pos (first subst) (inc pos) (first (rest subst)) )))

  )

(defn subst-pos-tpl [subst sol pos]

  (reverse (into () (assoc (into [] sol ) pos (first subst) (+ 1 pos) (charAtPos subst 1 0) (+ pos 2) (charAtPos subst 2 0) )))

  )


(defn all-single [subst sol pos]

  (cond

   (empty? pos) nil
   :else (concat

          (cond
           (empty? subst) nil
           :else (cons (subst-pos (first subst) sol (first pos)) (all-single (rest subst) sol pos))
           )
          (all-single subst sol (rest pos))

          )

   )

  )


(defn all-double [subst sol pos]

  (cond

   (empty? pos) nil
   :else (concat

          (cond

           (empty? pos) nil
           :else (concat

                  (cond
                   (empty? subst) nil
                   :else (cons (subst-pos-dbl (first subst) sol (first pos)) (all-double (rest subst) sol pos))
                   )
                  (all-double subst sol (rest pos))

                  )

           )

          )

   ))


(defn all-triple [subst sol pos]

  (cond

   (empty? pos) nil
   :else (concat

          (cond

           (empty? pos) nil
           :else (concat

                  (cond
                   (empty? subst) nil
                   :else (cons (subst-pos-tpl (first subst) sol (first pos)) (all-triple (rest subst) sol pos))
                   )
                  (all-triple subst sol (rest pos))

                  )

           )

          )

   )

  )


(defn make-distict [subst sol pos]


  (let [substitutions (all-double subst sol pos)]

    (display (distinct (map apply-rule-map substitutions)))

    )

  )


(def var1 '((ta . tum ta ta tum tum ta)
(ta (ta te) ta ta ta tum tum ta)
(ta (te ta) tum ta ta tum tum ta)
(ta (tum ta) tum ta ta tum tum ta)
(ta ta tum ta ta tum tum ta)
(ta tum ta ta ta tum tum ta)
(ta tum tum . ta tum tum ta)
(ta tum tum (ta te) ta tum tum ta)
(ta tum tum (te ta) tum tum tum ta)
(ta tum tum ta . tum tum ta)
(ta tum tum ta ta . tum ta)
(ta tum tum ta ta (ta te) ta ta)
(ta tum tum ta ta (te ta) tum ta)
(ta tum tum ta ta (tum ta) tum ta)
(ta tum tum ta ta ta tum ta)
(ta tum tum ta ta tum ta ta)
(ta tum tum ta ta tum tum (ta te))
(ta tum tum ta ta tum tum (te ta))
(ta tum tum ta ta tum tum (tum ta))
(ta tum tum ta ta tum tum ta)
(ta tum tum ta tum tum tum ta)
(ta tum tum tum ta tum tum ta)
(tum tum tum ta ta tum tum ta)))


(def var2 '(((ta te) ta tum tum ta tum tum ta)
(ta . tum tum ta tum tum ta)
(ta (ta te) ta tum ta tum tum ta)
(ta (te ta) tum tum ta tum tum ta)
(ta (tum ta) tum tum ta tum tum ta)
(ta ta (te ta) tum ta tum tum ta)
(ta ta (tum ta) tum ta tum tum ta)
(ta ta ta tum ta tum tum ta)
(ta ta tum (ta te) ta tum tum ta)
(ta ta tum (te ta) tum tum tum ta)
(ta ta tum ta ta tum tum ta)
(ta ta tum tum . tum tum ta)
(ta ta tum tum (ta te) ta tum ta)
(ta ta tum tum (te ta) tum tum ta)
(ta ta tum tum (tum ta) tum tum ta)
(ta ta tum tum ta tum . ta)
(ta ta tum tum ta tum (ta te) ta)
(ta ta tum tum ta tum ta ta)
(ta ta tum tum ta tum tum .)
(ta ta tum tum ta tum tum (ta te))
(ta ta tum tum ta tum tum (te ta))
(ta ta tum tum ta tum tum (tum ta))
(ta ta tum tum ta tum tum ta)
(ta tum tum tum ta tum tum ta)
(te ta tum tum ta tum tum ta)
(tum ta tum tum ta tum tum ta)))


(def var1-dbl '(((ta te) (ta te) ta ta ta tum tum ta)
((te ta) (te ta) tum ta ta tum tum ta)
((tum ta) (te ta) tum ta ta tum tum ta)
((tum ta) (tum ta) tum ta ta tum tum ta)
(ta (ta te) (ta te) ta ta tum tum ta)
(ta (ta te) ta ta ta tum tum ta)
(ta (te ta) (te ta) tum ta tum tum ta)
(ta (tum ta) tum ta ta tum tum ta)
(ta ta (ta te) ta ta tum tum ta)
(ta tum (ta te) (ta te) ta tum tum ta)
(ta tum (ta te) (ta tum) ta tum tum ta)
(ta tum (tum ta) (te ta) tum tum tum ta)
(ta tum (tum ta) (tum ta) tum tum tum ta)
(ta tum ta (ta te) ta tum tum ta)
(ta tum ta (tum ta) tum tum tum ta)
(ta tum tum . ta tum tum ta)
(ta tum tum (ta te) (ta te) ta tum ta)
(ta tum tum (ta te) ta tum tum ta)
(ta tum tum (te ta) (te ta) tum tum ta)
(ta tum tum (tum ta) (te ta) tum tum ta)
(ta tum tum ta (te ta) (te ta) tum ta)
(ta tum tum ta (tum ta) (te ta) tum ta)
(ta tum tum ta (tum ta) (tum ta) tum ta)
(ta tum tum ta ta (tum ta) tum ta)
(ta tum tum ta ta tum (ta te) (ta te))
(ta tum tum ta ta tum (ta te) ta)
(ta tum tum ta ta tum (te ta) (te ta))
(ta tum tum ta ta tum (tum ta) (te ta))
(ta tum tum ta ta tum (tum ta) (tum ta))
(ta tum tum ta ta tum ta (ta te))
(ta tum tum ta ta tum tum (ta te))
(ta tum tum ta tum . tum ta)
(ta tum tum tum . tum tum ta)
(tum . tum ta ta tum tum ta)
(tum (ta te) ta ta ta tum tum ta)))

(def var1-tpl '(((ta te) (ta te) (ta te) ta ta tum tum ta)
(ta (ta te) (ta te) (ta te) ta tum tum ta)
(ta (ta te) (ta te) ta ta tum tum ta)
(ta ta (ta te) (ta te) ta tum tum ta)
(ta tum . . ta tum tum ta)
(ta tum (ta te) (ta te) ta tum tum ta)
(ta tum tum . . tum tum ta)
(ta tum tum ta ta tum (ta te) (ta te))
(tum . . ta ta tum tum ta)
(tum (ta te) (ta te) ta ta tum tum ta)))

(defn ret-subst [sol pos]

  (cond

   (empty? pos) nil
   (list? (charAtPos sol (first pos) 0)) (concat (charAtPos sol (first pos) 0) (ret-subst sol (rest pos))  )
   :else (cons (charAtPos sol (first pos) 0) (ret-subst sol (rest pos))  )

   )

  )

(defn disp-pos-var [var]


  (cond

   (empty? var) nil
   :else (let [ pos (positions (first var)) sol (first var) ]

           (println pos (ret-subst sol pos) )
           (disp-pos-var (rest var))
           )
   )

  )

;(disp-pos-var var1)
                                        ;(disp-pos-var var2)

(disp-pos-var var1-dbl)
(disp-pos-var var1-tpl)

;make single, double, triple susbstitutions for the

;same pattern,  and
;different patterns,

;come up with the weighting function
;( double taps are less weighted than other substitions in beat just before accented positions than in other positions)
;( double taps are less/equally weighted as other substitutions in last beat than in other positions)

;with the same set of substitutions

                                       ;what is the significance of the positions that the variations mostly occur in

; significance in the bar, significant in the groove, (loudness, accent, start, end, middle beat)

; based on position in the bar

(double taps are more weighted as other substitutions in last beat)

(first beat)
(1 double taps are less weighted in the first beat than in the 2nd)

(2 double taps is more weighted in the first beat than 1 double tap)

(3 double taps is equally weighted as 2 double taps in the first beat)

(second beat)

(symm pattern, same as sixth beat)
(1 double tap less weighted as 1 double in 1st)

(2 double tap same as weighted on 1st beat)

(3 double taps weighted more than 1st beat)


(third beat)

(symm pattern, same as 7th beat)

(1 double beat > 2 double beats > 3 double beats)

(1 double beat < other substitutions)

(fourth beat)

(1 double taps are eqaully weighted than other substitutions)

(2 double equal as other substitutions)

(3 double more than as other substitutions)

(fifth)

(symm pattern, rules are same as first beat)


(sixth beat)

(2 double tap more weighted than 1 double tap )

(1 double tap equal weighted to other substitutions)

(3 double tap same as 1 double tap)


(seventh beat)

(1 double tap is weighted more than other substitutions)

(2 double tap less weighted than 1 double tap but equal to other substitutions)


(eight beat)

(1 double taps are eqaully weighted than other substitutions)

(2 , 3 double taps not possible)


;based on the accent

(The beat following an accent is usually accepted for variation)

(typically there are 2, 3 4 accents in a bar)

when there are - (3-3-2), (2-3-3),(3-2-3) configurations are possible
when are 2 accents (4-4)
4 accents, (2-2-2-2)

;;playing single <1 double < 2 double< 3 double

(for every accents, there are beats before and after the accented beat)

;(on the accent beat that falls in the first position)

(1 double tap > 2, 3 double taps)

(2,3 double taps = single tap)

(on other accent beats)

(2,3 > 1 double tap > single subst)

(3 double taps > all other costs)

;(beat following the accent)

(1 double beat weight = single beat weight)

(2 double beats)

;(2 double beats can either end 1 or 2 beat before next accent )

(if they end 1 beat before next accent)
(2 double beat weight = 1 double beat weight)
:else (2 double beat weight > 3 double taps)

(3 double taps)

(if they end 1 beat before next accent)
(3 double beat weight = 1 double beat weight)
:else (3 double beat weight > 2 double taps)

;(beat before the accent)

(1 double beat weight = single beat weight)

( 2,3 double beat > far greater)



(2 double beat = 1 double beat only for 1st beat)


(double beat < triple in beat following accent)
