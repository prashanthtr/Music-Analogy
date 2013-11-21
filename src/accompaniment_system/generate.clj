;This program uses multiple rules to generate different possible improvisations. It does it by first identifying the parts of the lead that can be improvised and substitutes the appropriate improvisatory action for those parts.


;------------- Utitlies functions -------------

;returns length of a list

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

;;neatly displays

(defn display [list]

  (cond

   (empty? list) nil
   :else (do (println (first list)) (display (rest list)))

   )

  )


(defn random-subst [list pos]

  ( charAtPos list (rand-int (lengthList list 0 )) 0 )

  )



;------------- variables and substitution -------------


;;substitues corresponding "variable" for pause, double taps and accented or non accented positions, based on the "subst" mapping

(defn var-sub [ Sol pos st subst]

  ( cond

    (>= st (lengthList Sol 0)) nil
    (= -1 (.indexOf pos st)) (cons (charAtPos Sol st 0) (var-sub Sol pos (+ st 1) subst))
    :else  (cons (get subst (charAtPos Sol st 0)) (var-sub Sol pos (+ st 1) subst))
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



;;finds positions from newsolthat contains '?p, '?nA etc
(defn findpos [sol st]

  (cond

   (empty? sol) nil
   (variable (first sol)) (cons st (findpos (rest sol) (+ st 1)))
   :else (findpos (rest sol) (+ st 1))
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

; Replace each mridangam hit with kanjira hit

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

;------------- Physical constraints -------------

;;changes the hit in the case of continuous repetition of the same hit even after double tap. current code distinguishes between te and tum as hits on separate finger, but putting them on same finger will reduce the number of possible double hits

(defn changesol [sol]

  (cond

   (or (= sol 'te) (= sol 'tum)) 'ta
   :else 'tum

   )

  )


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


;; identifies the unchangable or forced hits in a 8 beat groove based on the accent position

(defn notChangeRule [doublehit accent]

  (cond

   (empty? doublehit) nil
   (not= (.indexOf accent (first doublehit)) -1 ) (notChangeRule (rest doublehit) accent )
   :else (cons (first doublehit) (notChangeRule (rest doublehit) accent ) )

   )

  )

;; code that selects the positions to introduce double taps and the double tap to replace, It uses the above simple rules to adjust the subsequent notes in the generated double taps

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

;; double taps is constraining factor
;; pauses, non accented hits enhance options to improvise

;; double tap in the beginning constraints the rest of the groove
;; double tap is directive of secondary's improvisation as it highlights the points around which variations can be done
;; no double taps offers slightly larger scope for improv


;;generates a seconary response to lead based on a given lead, secondary pattern, accent structure, substition variables and possible improvisational substitions.

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


;; ta te followed by pause is awkard, hence is usually played with a tum
(defn tatetumRule [ksol st]

  (cond

   (>= st (lengthList ksol 0)) nil
   ( and (= '. (charAtPos ksol st 0) ) (= '(ta te) (charAtPos ksol (- st 1) 0) ) ) (cons (random-subst '(tum ta) 0) (tatetumRule ksol (+ st 1)) )
   :else (cons (charAtPos ksol st 0) (tatetumRule ksol (+ st 1)) )
   )


  )


; Function that applies all the physical constraint rules on a sol to make it correct

(defn apply-rule-map [ksol]

  (tatetumRule (tumteRule (doubleteta (cons (first ksol) (ruleDouble (first ksol) (first (rest ksol)) (rest ksol) ))) 0) 0)

  )



;------------- Substituting the variable positions  -----------------

(defn single-level-subst [hit pos]

  (random-subst '(tum ta te (ta te) (tum ta) (te ta) .) 0)

;(cond

   ;(= pos 0) (random-subst '(tum ta te (ta te)) pos) ;;first position
 ;  (list? hit) (random-subst '(tum (ta te) .) pos)
  ; (resonant hit) (random-subst '(tum (ta te) .) pos)
  ; :else (random-subst '(ta te (ta te) .) pos)
   ;)

  )

(defn double-level-subst [pos]

  ;(cond

   ;(not= 0 pos) (random-subst '(((ta te) tum) ((ta te) (ta te)) (tum .)) pos)
  ; :else (random-subst '((tum (ta te) ) (tum tum) ((ta te) (ta te)) (tum .)) pos)

                                        ;)

  (random-subst   '((tum (ta te) ) (tum tum) ((ta te) (ta te)) (tum .) ((ta te) tum) ((ta te) ta) (ta (ta te)) ((tum ta) tum) (tum (tum ta)) (ta (tum ta)) (ta (ta te) ) (tum (te ta))) 0)

  )

(defn third-level-subst [pos]

  (random-subst '((tum tum tum) (tum ta tum) ((te ta) (te ta) (te ta)) ((ta te) (ta te) (ta te))) 0)

  ;(cond

                                        ;   (not= 0 pos) (random-subst '(((ta te) (ta te) (ta te))  ((te ta) (te ta) (te ta)) ) pos)
                                        ;  :else (random-subst '((tum tum tum) (tum ta tum) ((te ta) (te ta) (te ta)) ((ta te) (ta te) (ta te))) pos)

                                        ;)

   )


(defn ret-op [var]

  (cond

   (list? var) concat ;(do (println var "concat") concat)
   :else cons ;(do (println var "cons") cons)
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

;; returns all single substtititons given in the subst array

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

;; returns all double substtititons given in the subst array
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


;; returns all triple substitions guven in the the subst array
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


(defn ret-subst [sol pos]

  (cond

   (empty? pos) nil
   (list? (charAtPos sol (first pos) 0)) (concat (charAtPos sol (first pos) 0) (ret-subst sol (rest pos))  )
   :else (cons (charAtPos sol (first pos) 0) (ret-subst sol (rest pos))  )

   )

  )



; ------------- consolidate function that substitutes the variable positions  -----

;; has to be multie layer recursive, for each substitution, look forward for all the alternate, single double and triple substitions and return the result.
;;substitute double or triple only if there are so many variable positions

;; all the nodes that can be activated from the current concept
;; all the groove that are meaningful variations of the current groove


;; gives priority to double if there is a chance of the double substitution
;; gives priority to triple if there is a chance of triple substition

;;makes substitution taking into account the speed of the hits in the substitution
(defn multi-level-subst [sol st]

                                        ; single subst
                                        ;double subst
                                        ;triple subst

  (if (>= st (lengthList sol 0))

    nil
    (let [hit (charAtPos sol st 0)]

      (cond

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


; ------------- Generation  -----

;; Takes in a forced choice with the variable positions, a particlar interpretation of the forced choice ( accent positions), substitution array and displays the possible choices generated from it

(defn generation-multi-subst [forced-sol comb-pos subst st]

  (cond

   (empty? comb-pos) nil
   :else
   (let [pos (first comb-pos)
         newSol (var-sub (apply-rule-map forced-sol) pos 0 subst)
         ]

     ;(println newSol)
     (cond

      (< st 100) (distinct (cons (multi-level-subst newSol 0) (generation-multi-subst forced-sol comb-pos subst (+ st 1)) ))
      :else (generation-multi-subst forced-sol (rest comb-pos) subst 0)
      )

     )

   )

  )


(defn gen-subsumption [Sol accent subst]


  (let [ pos (sort (notChangeRule (positions Sol) accent) )
        pos2 (distinct (concat (positions Sol) (nonAccentPos Sol accent 0 ) ))
        newSol (var-sub (apply-rule-map Sol) pos 0 subst)

        ]

    (cond

     (empty? pos) (multi-level-subst2 Sol pos2)
     :else (multi-level-subst2 Sol pos)

     )

    ;(distinct (concat (positions Sol) (nonAccentPos Sol accent 0 ) ))
    ;(notChangeRule (distinct (concat (positions Sol) (nonAccentPos Sol accent 0 ) )) accent)
    ;(println "pattern" newSol)

;   (cond

                                        ; (>= st 3) nil
                                        ; :else

                                        ;    )
    ;substSol
    )
  )

;(main '(num dhin . dhin num dhin . dhin) '(0 1 2 3 4 5 7) {'. '?p '(ta te) '?d '(te ta) '?d '(ta tum) '?d '(tum tum) '?d '(tum ta) '?d  'tum '?nA 'ta '?nA 'te '?nA} )

;; Takes in lead mridangam, multiple combination arrays( each is an interpretation of the lead) and substition mappings and generates valid substitions according to the rules

(defn all-combinations [list st]

  (cond

   (> st (lengthList list 0)) nil
   :else (concat (combinations list st) (all-combinations list (+ st 1))  )
   )

  )

(defn main [mSol comb subst]

  (cond

   (empty? comb) nil
   :else (let [accent (first comb) pos (sort (notChangeRule (positions mSol) accent) ) newSol (mriMap mSol)
                                        ;(var-sub (apply-rule-map (mriMap mSol)) pos 0 subst)
               ;(notChangeRule (distinct (concat (positions mSol) (nonAccentPos mSol accent 0 ) )) accent)
               ]
           (println "main" pos newSol (all-combinations pos 0))
           (let [
                 comb-pos (all-combinations pos 0)
                 output (map apply-rule-map (concat (generation-multi-subst newSol comb-pos subst 0) (main mSol (rest comb) subst)))]

             (distinct output)

             )

           )
   )

  )

;-- other ways to generate

(defn make-distict [subst sol pos]

  (let [substitutions (all-double subst sol pos)]

    (display (distinct (map apply-rule-map substitutions)))

    )

  )


;;calls main multiple times to do different possible substitutions and return distinct substiutions


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



(defn disp-pos-var [var]


  (cond

   (empty? var) nil
   :else (let [ pos (positions (first var)) sol (first var) ]

           (println pos (ret-subst sol pos) )
           (disp-pos-var (rest var))
           )
   )

  )
