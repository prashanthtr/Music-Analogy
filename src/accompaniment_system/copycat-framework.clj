(load-file "src/accompaniment_system/generate.clj")


;; input : sol

;; output: possible groupings and what they are grouped based on

;;groups the groove based on double hits

(defn doublehit-group [sol st]

  (cond

   (>= st (lengthList sol 0)) nil
   (and (list? (charAtPos sol st 0)) (list? (charAtPos sol (+ st 1) 0)) (list? (charAtPos sol (+ st 2) 0))) (cons (list (charAtPos sol st 0) (charAtPos sol (+ st 1) 0) (charAtPos sol (+ st 2) 0) ) (doublehit-group sol (+ st 3)) )
   (and (list? (charAtPos sol st 0)) (list? (charAtPos sol (+ st 1) 0)) ) (cons (list (charAtPos sol st 0) (charAtPos sol (+ st 1) 0) ) (doublehit-group sol (+ st 2)) )
   (list? (charAtPos sol st 0)) (cons (list (charAtPos sol st 0)) (doublehit-group sol (+ st 1))  )
   :else (cons (charAtPos sol st 0) (doublehit-group sol (+ st 1))  )
   )

  )

;;groups the groove based on accents
(defn accent-group [sol accent]

  (cond

   (empty? accent) nil
   (empty? (rest accent)) (cons (subsequ sol (first accent) (- (lengthList sol 0) 1) 0 )  (accent-group sol (rest accent)) )
   (= 0 (first accent)) ( cons (subsequ sol (first accent) (- (first (rest accent)) 1) 0 ) (accent-group sol (rest accent) ) )
   :else ( concat (subsequ sol (first accent) (- (first (rest accent)) 1) 0 ) (accent-group sol (rest accent) )   )

   )

  )

(defn doubleinaccent [sol]

  (cond

   (empty? sol) nil
   :else (cons (doublehit-group (first sol) 0) (doubleinaccent (rest sol)))

   )

  )


;; does the order bonds
(defn interpretations [sol accent]

  (cond

   (empty? accent) nil
   :else

   (let [accentgrp (cond
                    (= (first (first accent)) 0) (accent-group sol (first accent))
                    :else (accent-group sol (cons 0 (first accent)))
                    )
         dblgroup (doubleinaccent accentgrp)
         ]
     ;(println accentgrp)
     (cons dblgroup (interpretations sol (rest accent)) )
     )

   )

  )

(defn check-len [sol]

  ;(println sol)
  (> (lengthList sol 0) 1) (check-len (first sol))
  :else sol
  )

;; bind a variable to a pattern and find if the same binding holds for the rest of the pattern, else extend the binding to include next element and check the rest of the pattern

(defn check-binding [var list binding]

  ;(println list binding)
  (let [bind (get binding var)]

    (cond

     (not= list bind) false
     :else true
     )
    )
  )

(defn check-binding-compl [var list binding]

  ;(println list binding)
  (let [bind (get binding var )]

    (cond
     (not= (reverse list) bind) false
     :else true
     )
    )
  )

(defn symm [list bindings st var chk-binding]

  (let [;list (check-len sol)
        seq (subsequ list 0 st 0)
        binding {var seq}
        ]

    ;(println list)
    ;(println binding)
    (cond

     (>= st (lengthList list 0)) bindings
     (chk-binding var (subsequ list (+ st 1) (- (lengthList list 0) 1) 0) binding) (merge binding bindings)
     :else (symm list bindings (+ st 1) var chk-binding)

     )

    )

  )

;;function returns description of the position of the double hit in the beginning, middle and end of the pattern
;beginning 1 beat - first
;middle 2 beats - (first last)
;end last


;;builds description of double hits in the pattern

(defn doublehit [sol bindings var]

  (let [
        b1 (cond

            (list? (first sol)) (merge bindings {(cons '(first) var) (first sol) })
            :else bindings
            )
        b2 (cond

            (list? (last sol)) (merge b1 {(cons '(last) var) (last sol) })
            :else b1
            )
        len (lengthList sol 0)
        halflen (/ len 2)
        mid (subsequ sol (- halflen 1) halflen 0)
        middle '(middle)
        b3 (cond

            (and (list? (first mid)) (list? (first sol)) ) (merge b2 {(cons (cons '(whole) middle) var) mid})
            (list? (first mid)) (merge b2 {(cons (cons '(first) middle) var) (first mid) })
            (list? (last mid)) (merge b2 { (cons (cons '(last) middle) var) (last mid)})
            :else b2
            )

        ]
    b3
    )

  )


;; takes a pattern and finds the symmetry across whole, and half patterns
(defn sym-sol [sol bindings]


  (let [
        firsthalf (subsequ sol 0 3 0)
        secondhalf (subsequ sol 4 (- (lengthList sol 0) 1) 0)
        cb check-binding
        ccb check-binding-compl
        b1 (symm sol bindings 0 '(same whole) cb )
        b2 (symm firsthalf b1 0 '(same first) cb)
        b3 (symm secondhalf b2 0 '(same second) cb)
        b1 (symm sol bindings 0 '(opp whole) ccb )
        b2 (symm firsthalf b1 0 '(opp first) ccb)
        b4 (symm secondhalf b2 0 '(opp second) ccb)
        b1 (doublehit sol bindings '(dh whole))
        b2 (doublehit firsthalf b1 '(dh first))
        b5 (doublehit secondhalf b2 '(dh second))
        ]
    (merge b3 b4 b5)
    )

  )


(defn relationBetween [s1 s2]

  ;(println s1 s2)

  (cond
   (nil? s1) 'NR
   (nil? s2) 'NR
   (= s1 s2) 'I

   (and (= s1 'first) (= s2 'second) ) 'successor   (and (= s1 'second) (= s2 'first)) 'predecessor

   (and (= s1 'first) (= s2 'last) ) 'successor
   (and (= s1 'last) (= s2 'first)) 'predecessor

   (and (= s1 '(middle (last))) (= s2 '(first))) 'predecessor
   (and (= s2 '(middle (last))) (= s1 '(first))) 'sucessor
   (and (= s1 '(middle (first))) (= s2 '(last))) 'sucessor
   (and (= s2 '(middle (first))) (= s1 '(last))) 'predecessor

   (and (= s1 '(middle (whole))) (= s2 '(middle (first))) ) 'rev-supplementary
   (and (= s1 '(middle (first))) (= s2 '(middle (whole))) ) 'supplementary
   (and (= s1 '(middle (whole))) (= s2 '(middle (last))) ) 'rev-supplementary
   (and (= s1 '(middle (last))) (= s2 '(middle (whole))) ) 'supplementary

   ;; middle and no middle is also supplementary

   :else 'NR ;no relation

   )

  )

(defn relationSol [sol1 sol2]

  (cond

   (= sol1 sol2) 'same
   (= sol1 (reverse sol2)) 'opp
   )

  )


;Defines the relation between same types of object descriptions. Same in the sense of number of beats they describe is the same

; returns a string with NR if no relation is possible between any 2 descriptors

;;handle error

(defn relationDesc [desc1 desc2]

  (cond

   (empty? desc1) nil
   :else (let [
               b1 (relationBetween (first desc1) (first desc2))
               ]

           (cond

            (= 'nil b1) nil
            :else (cons b1 (relationDesc (rest desc1) (rest desc2)))
            )
           )

   )

  )



(defn desc-matching [ sol1-desc sol2-desc]

  (cond

   (empty? sol1-desc) nil
   :else (let [
               desc (nth (first sol1-desc) 0)
               ]
           (cond

            (not= nil (get sol2-desc desc)) (do (println "bridge formed" desc) (desc-matching (rest sol1-desc) sol2-desc) )
            :else (desc-matching (rest sol1-desc) sol2-desc)
            )

           )
   )

  )



(defn descriptorMatch [desc1 desc2]

  (cond

   (empty? desc1) nil
   (= desc1 desc2) 'I
   (= (rest desc1) (rest desc2)) (let [
                                       relation ( relationBetween (first desc1) (first desc2))
                                       ]
                                   (cond
                                    (= relation 'NR) 'NR
                                    :else relation
                                    )

                                   )
   (= (rest (rest desc1)) (rest (rest desc2)) ) (let [
                                                      r1 ( relationBetween (first (rest desc1)) (first (rest desc2)))
                                                      ]
                                                  ;(println r1)
                                   (cond
                                    (= r1 'NR) 'NR
                                    :else r1
                                    )

                                   )
   :else (let [
               r2 ( relationBetween (first (rest (rest desc1))) (first (rest (rest desc2))))
               ]
           ;(println r2)
           (cond
            (= r2 'NR) 'NR
            :else r2
            )

           )

   )


  )

;creates bridges between descriptor notatios
(defn bridges [desc1 desc2 bridge]

  (cond

   (empty? desc1) bridge
   :else (cond

          (empty? desc2) bridge
          :else (merge
                 (let [
                       rel (descriptorMatch (first desc1) (first desc2))
                       ]
                   (cond

                    (= rel 'NR) (bridges desc1 (rest desc2) bridge)
                    :else (bridges desc1 (rest desc2) (merge bridge {(list (first desc1) (first desc2)) rel}))

                    )

                   )
                 (bridges (rest desc1) desc2 bridge))
          )
   )
  )

(defn joinDescSol [desc sol]

  (cond

   (empty? sol) nil
   :else (cons (list (first desc) (first sol)) (joinDescSol (rest desc) (rest sol)))

   )

  )

(defn deeprev [sol]



  (let [
        f (first sol)
        ]
    ;(println f sol (seq? f))
    (cond

     (empty? sol) nil
     (seq? f) (cons (reverse f) (deeprev (rest sol)))
     :else (cons f (deeprev (rest sol)))
     )

    )


  )

(defn setToList [set]

  ;(println set)
  (let [
        desc1 (map reverse (map first (into [] set) ))
        desc (map deeprev desc1)
        sol (map last (into [] set))
        joined (joinDescSol desc sol)
        ]

    ;(println joined)
    joined
    )

  )

; checks and creates the symmetry bindings if any for each of the list
(defn symm-bonds [lists]

  (cond

   (empty? lists) nil
   :else  (cons (sym-sol (first lists) {}) (symm-bonds (rest lists)) )
   )

  )



(defn desc-var [pos]

  (cond

   (= pos 0) (list '(((first) dh whole)) '(((first) dh first)))
   (= pos 1) '( (((first) middle) dh first) )
   (= pos 2) '( (((last) middle) dh first) )
   (= pos 3) (list '( (((first) middle) dh whole) ) '(((last) dh first) ))
   (= pos 4) (list '((((last) middle ) dh whole)) '(((first) dh second)))
   (= pos 5) '((((first) middle) dh second))
   (= pos 6) '((((last) middle) dh second))
   (= pos 7) (list '(((last) dh whole)) '(((last) dh second)))
   :else nil

   )

  )

(defn listtoEmptySet [list]

  {list ()}

  )

(defn lists-to-set [lists]

  (cond

   (empty? lists) nil
   :else (let [
               sol (first lists)
               ]
           (cond
            (> (lengthList sol 0) 1) (merge (lists-to-set sol) (lists-to-set (rest lists)) )
            :else (merge (listtoEmptySet sol) (lists-to-set (rest lists))  )
            )

           )

   )

  )


(defn var-discr-descr [var-pos]

  (let [
        desc (map desc-var var-pos)
        descr (lists-to-set desc)
        d1 (map first (into [] descr) )
        d2 (map first d1)
        sol (map last (into [] descr))
        joined (map hash-map d2 sol)
        ]
    (into {} joined)
    )

  )

;;creates structures also taking into account the possible variable opsitions

;;stub
;(createStruct '((ta tum tum ta ta tum tum ta) (ta tum tum (ta te) ta tum tum ta)) '(0 4) )


(defn singlify [list]

  (cond

   (empty? list) nil
   :else ( concat (first list) (singlify (rest list)))

   )

  )


(defn make-set [el1 el2]

  {el1 el2}

  )

(defn make-set-lists [list]

  (cond

   (empty? list) nil
   :else (merge (make-set (first list) (first (rest list))) (make-set-lists (rest (rest list)) )   )

   )

  )

(defn iter-sol [d1 desc]

  (cond

   (empty? desc) nil
   :else (let [
               sol (get d1 (first desc))
               ]
           (cond

            (= sol nil) (iter-sol d1 (rest desc))
            :else (cons sol (iter-sol d1 (rest desc)))
            )
           )


   )

  )

(defn setToList-gen [set]

  ;(println set)
  (let [
        desc (map reverse (map first (into [] set) ))
        sol (map reverse (map last (into [] set)))
        desc (singlify desc)
        ]
   ; (println "desc" desc)
    ;(println "sol" (map reverse sol))
    (map reverse sol)
    )

  )

(defn relation-hits [h1 h2]

  ;(println "reln hits" h1 h2)
  (cond
   (and (list? h1) (list? h2) (empty? h1)) 'supplementary
   (and (not (list? h1)) (list? h2)) 'supplementary
   (and (list? h1) (not (list? h2))) 'rev-supplementary
   (and (list? h1) (list? h2) (= h1 h2)) 'I
   (= h1 h2) 'I
   (and  (not (list? h1)) (not (list? h2)) (not= h1 h2)) 'I
   :else 'NR
   )

  )


(defn relation-sol-hits [sol1 sol2]

  ;(println "relnsolhits" sol1 sol2)
  (cond

   (empty? sol2) nil
   :else (cons (relation-hits (first sol1) (first sol2)) (relation-sol-hits (rest sol1) (rest sol2))  )

   )
  )

(defn relation-sol [sol1 sol2]

  ;(println "reltion sol" sol1 sol2)
  (let [
        relation (relation-sol-hits sol1 sol2)
        ]

    (cond

     (or (= relation '(I I I I)) (= relation '(I I))) 'I
     (or (= relation '(I I I supplementary)) (= relation '(I supplementary))) 'supplementary
     (or  (= relation '(supplementary I I I))  (= relation '(supplementary I))) 'supplementary
     (or (= relation '(I I I rev-supplementary)) (= relation '(I rev-supplementary))) 'rev-supplementary
     (or  (= relation '(rev-supplementary I I I))  (= relation '(rev-supplementary I))) 'rev-supplementary
     :else 'NR
     )

    )

  )

(defn bridges-hits[t1 t2 h1 h2 bridges]

  (cond

   (empty? h1) bridges
   (= nil (first h2)) (bridges-hits (rest t1) (rest t2) (rest h1) (rest h2) bridges)
   :else (let [
               rln (cond

                     (and (seq? (first h2)) (seq? (first h2))) (relation-sol (first h1) (first h2))
                     :else  (relation-hits (first h1) (first h2))
                     )

               ]
           (cond

            (= rln 'NR) (bridges-hits (rest t1) (rest t2) (rest h1) (rest h2) bridges)
            :else (bridges-hits (rest t1) (rest t2) (rest h1) (rest h2)
                                (merge bridges
                                       {(list (first t1) (first t2) ) rln}
                                       )
                                )
            )

           )
   )

  )

(defn form-var-connections [t1 t2 d1 d2 bridges]

  (cond

   (empty? t1) nil
   :else
   (let [
         sol1 (get d1 (first t1))
         sol2 (get d2 (first t1))
         rln (cond


              (and (seq? sol1) (seq? sol2)) (relation-sol sol1 sol2)
              :else  (relation-hits sol1 sol2)
               )
         ]
     (cond
       (= sol2 nil) (form-var-connections (rest t1) (rest t2) (rest d1) (rest d2) bridges)
      (= rln 'NR) (form-var-connections (rest t1) (rest t2) (rest d1) (rest d2) bridges)
      :else (form-var-connections (rest t1) (rest t2) (rest d1) (rest d2)
                                  (merge bridges
                                         {(list (first t1) (first t1) ) rln}
                                       )
                                  )
      )
     )

   )

  )

(defn iter-sol2[set list output]

  (cond

   (empty? list) nil
   (= nil (get set (first list))) (iter-sol2 set (rest list) output)
   :else (iter-sol2 set (rest list) (merge output {(first list) (get set (first list))}  ) )
   )

  )


(defn reln-hits [d1 d2 bridges]

  ;(println "bridges" bridges)
  ;(println "d1" d1)
  ;(println "d2" d2)
  (let [
        t1 (map first d1)
        ;d1-sol (iter-sol2 d1 t1)  ;contains the forced desc related to disc
        d2-sol (iter-sol2 d2 t1 {}) ;contains the discr desc related to forced

        ]
    (println "bride var hits" )
    (println d2-sol)
    (println "bridges with var" (merge bridges d2-sol))
    (merge bridges d2-sol)
;    (println (bridges-hits t1 t2 d1-sol d2-sol {}))
 ;   (bridges-hits t1 t2 d1-sol d2-sol {})
    )

  )


(defn list-to-set [list set]

  (cond

   (empty? list) set
   :else (list-to-set (rest list) (merge set (first list)))
   )

  )

;(reln-hits '{(same whole) (ta tum tum ta), (opp first) (ta tum), (opp whole) (ta tum tum ta), (opp second) (ta tum), (((last) middle) dh whole) (), ((first) dh second) (), ((first) dh first) (), ((first) dh whole) ()} )


;in the case of similar descriptors(complementary), system ignores the
;in the case of unsimilar descriptors, system takes the hits into account

;next step is to make sense of these connections and use them to select from amongst the discretionary

(defn hit-relation [common set1 set2 relatn]

  ;(println "enter hit-relation" relatn)
  (cond
   (empty? common) relatn
   :else (let [
               h1 (get set1 (first common))
               h2 (get set2 (first common))
               reln (cond

                     (and (seq? h1) (seq? h2)) (relation-sol h1 h2)
                     :else (relation-hits h1 h2)
                     )



                ]
            ;(println "relantion" h1 h2 reln)
            (cond

             (= reln 'NR) (hit-relation (rest common) set1 set2 relatn)
             :else (hit-relation (rest common) set1 set2 (merge { (list (first common) (first common) ) reln} relatn ) )
             )
            )
   )

  )

(defn bridge-same-desc [set1 set2]

  (let [
        desc1 (set (map first set1))
        desc2 (set (map first set2))
        common (intersection desc1 desc2)
        bridge-same (hit-relation (into () common) set1 set2 {})
        ]
    ;(println "common" common)
    bridge-same
    )

  )


(defn show-supplementary [set]

  (cond

   (empty? set) nil
   :else (let [
               identifier (first (first set))
               val (get (into {} set) identifier)
               ]
           ;(println identifier val)
           (cond

            (= val 'supplementary) (cons identifier (show-supplementary (rest set)) )
            :else (show-supplementary (rest set))
            )
           )
   )

  )



(defn show-identity [set]

  (cond

   (empty? set) nil
   :else (let [
               identifier (first (first set))
               val (get (into {} set) identifier)
               ]
           ;(println identifier val)
           (cond

            (= val 'I) (cons identifier (show-identity (rest set)) )
            :else (show-identity (rest set))
            )
           )
   )

  )

(defn singlifyset [list out]

  (cond

   (empty? list) out
   :else (singlifyset (rest list) (merge out (first list)))
   )

  )

(defn createStruct [lists var]

  (let [
        desc-sol (symm-bonds lists)
        forced-desc (first desc-sol)
        var-desc (var-discr-descr var)
        forced (merge var-desc forced-desc)
        forced-discr (cons forced (rest desc-sol))
        description (map setToList forced-discr)
        d1 (first description)
        d2 (last description)
        d1-set (make-set-lists (singlify d1))
        d2-set (make-set-lists (singlify d2))
        d1-desc (map first d1-set)
        d1-sol (map last d1-set)
        d2-desc (map first d2-set)
        d2-sol (map last d2-set)
        bridge-desc (bridges d1-desc d2-desc {})
                                        ;bridge between possible relations based on system's concept of predcessor, successor etc.(needed??)
        bridges-var (reln-hits d1-set d2-set bridge-desc) ;var hits in the lead
        merge-bridge (merge bridges-var (bridge-same-desc d1-set d2-set )  )
        ]

    ;(println desc-sol)
    ;(println d2-sol)
    ;(println "bridge" bridge-sol)
    ;(println "d1 set" d1-set)
    ;(println "d2 set" d2-set)
    (println bridges-var)
    ;(println bridge-sol)

                                        ;(bridge-same-desc d1-set d2-set )
    (println "bridges" merge-bridge)
    ;(println "show identity bridges" (show-identity (set merge-bridge)))
    ;(println "show supplementary bridges" (show-supplementary (set merge-bridge)))
    ;(merge (list-to-set bridge-sol {}) bridges-var )

    )

  )
