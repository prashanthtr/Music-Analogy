;; Contains production rules that give the output choices in diction

(def prod-rules

  (list
    {
     'relation 'sameness
     '(Q Q)
     {
      '(Q1 Q1) (list '( (ta ta) (te te) (tum tum) (. .)))
      '(Q1 Q2) (let [
                     a1 (combinations '(ta te tum .) 2)
                     a2 (map (partial reduce conj '()) a1 )
                     ]
                 (list (concat a1 a2))
                 )
      }
     }

    {
     'relation 'sameness
     '(EE EE)
     {
      '(E1E1 E1E1) (list '( ((ta ta) (ta ta)) ((te te) (te te)) ((tum tum) (tum tum)) ))
      '(E1E1 E1E2) (let [
                         a1 (combinations '(ta te tum .) 2)
                         a2 (map (partial reduce conj '()) a1 )
                         choices (list (concat a1 a2))
                         cp-choices (cartesian-product choices choices)
                         e1-choices   (map

                                       (fn [c1]

                                         (let [
                                               f (first c1)
                                               s (second c1)
                                               ]
                                           (cond

                                        ; E1E1
                                            (and (= (first f) (second f)) (not= (first s) (second f)))
                                            c1
                                            :else nil
                                            )
                                           )

                                         )
                                       cp-choices)
                         ]
                     (reduce concat e1-choices)
                     )

      '(E1E2 E1E2) (let [
                         a1 (combinations '(ta te tum .) 2)
                         a2 (map (partial reduce conj '()) a1 )
                         choices (list (concat a1 a2))
                         cp-choices (cartesian-product choices choices)
                         e1-choices   (map

                                       (fn [c1]

                                         (let [
                                               f (first c1)
                                               s (second c1)
                                               ]
                                           (cond

                                        ; E1E2
                                            (and (not= (first f) (second f)) (not= (first s) (second f)))
                                            c1
                                            :else nil
                                            )
                                           )

                                         )
                                       cp-choices)
                         ]
                     (reduce concat e1-choices)
                     )

      }
     }

    {
     'relation 'difference
     '(Q EE)
     {
      '(Q1 E1E1) (list '( (ta (ta ta)) (te (ta te)) (tum (tum tum)) ))
      '(Q1 E2E2) (list '( (ta (te te)) (ta (tum tum)) (te (ta ta)) (te (tum tum)) (tum (ta ta)) (tum (te te)) ))
      '(Q1 E1E2) (let [
                       a1 (combinations '(ta te tum .) 2)
                       a2 (map (partial reduce conj '()) a1 )
                       list-dh (concat a1 a2)
                       Qee    (map
                               (fn [d1]
                                 (map
                                  (fn [dh1]
                                    (cons d1 (list dh1))
                                    )
                                  list-dh)
                                 )
                               '(ta te tum))
                       ]
                   (map concat Qee)
                   )
      }
     }

    {
     'relation 'difference
     '(EE Q)
     {
      '(E1E1 Q1) (list '( ((ta ta) ta) ((ta te) te) ((tum tum) tum) ))
      '(E2E2 Q1) (list '(((te te) ta) ((tum tum) ta) ((ta ta) te) ((tum tum) te) ((ta ta) tum) ((te te) tum)))
      '(E1E2 Q1) (let [
                       a1 (combinations '(ta te tum .) 2)
                       a2 (map (partial reduce conj '()) a1 )
                       list-dh (concat a1 a2)
                       Qee    (map
                               (fn [d1]
                                 (map
                                  (fn [dh1]
                                    (cons d1 (list dh1))
                                    )
                                  list-dh)
                                 )
                               '(ta te tum))
                       eeQ (map
                            (fn [d1]
                              (map (partial reduce conj '()) d1 ) )
                            Qee)
                       ]
                   (map concat eeQ)
                   )
      }

     }
                                        ;('(Q Q) 'H) { '( '(Q1 Q1) 'H1) ('(Q1 Q2) 'H1)
                                        ;('(EE EE) 'H)


    )

  )
;relational
;function that takes in a descriptor like "multiply Q by 1/2"
;computes that Q * 1/2 is EE
;matching case: Q -> EE

;higher level production rule returns list of note duration for a particular Abstract transformation

(defn find-pr [sym1 sym2 prs]

  (cond

   (empty? prs) "no matching Production rule"
   (not= nil (get (first prs) (list sym1 sym2) ))
   (get (first prs) (list sym1 sym2) )
   :else (find-pr sym1 sym2 (rest prs))
   )

  )

(defn find-pr-maps [sym1 sym2 prs]

  (cond

   (empty? prs) "no matching Production rule"
   (not= nil (get prs (list sym1 sym2) ))
   (get prs (list sym1 sym2) )
   :else (do (println "no matching Production rule") nil)
   )

  )

;Mid level production rule returns list of diction choices for a particular Abstract transformation

(defn ml-pr [hls1 hls2 mls1 mls2 prs]

  (let [
        hlpr (find-pr hls1 hls2 prs)
        mlpr (find-pr-maps mls1 mls2 hlpr)
        ]
    (cond

     (= nil mlpr) nil
     :else (reduce concat mlpr)
     )
    ;(println mlpr)
    )
  )

;; using the diction on consecutive hits, generate list of alternate diction choices using the production rules. This will be part of finding the inverse

(defn matchDic [list sym]

  (cond

   (empty? list) nil
   (= (first (first list)) sym) (cons (second (first list)) (matchDic (rest list) sym))
   :else (matchDic (rest list) sym)
   )

  )


; Takes in 2 dictions, 2 note durations and 2 literals to find an appropriate match
; Idea is , however, something, if literals don't match, go upto to note durations and if note durations don't, go upto sameness/differences realtions etc.

(defn list-alternate-diction [rln prs & args] ;t1 t2 for ND and literal

  (println "relation" rln "ids" (first args) (second args) "Args" (first args))
  (cond

   (= (count args) 0) ; return all matching sameness/difference transitions
   (let [
         choices    (map

                     (fn [pr]
                       (cond
                        (= rln (get pr 'relation)) pr
                        :else nil
                        )

                       )
                     prs)
         choices (into {} (reduce concat choices))
         ; a selection that is not random ??
         ]
     choices
     )

   (= (count args) 2) ; return all matching sameness/difference transitions
   (let [
         choices    (map

                     (fn [pr]

                       (cond
                        (= rln (get pr 'relation)) pr
                        :else nil
                        )

                       )
                     prs)
         choices (into {} (reduce concat choices))
         ; a selection that is not random ??
;         (rln choices iD mD (list (get-ND-type iD) (get-ND-type mD)))
         ]
     choices
     )

   (= (count args) 3)
   (let [
         opt1 (first args)
         choices    (map

                     (fn [pr]

                       (println pr)
                       (cond
                        (and (= rln (get pr 'relation)) (not= nil (get pr opt1))) pr
                        :else nil

                        )
                       )
                     prs)
         ]
     ;(println "hello 2" opt1 choices)
     (into {} (reduce concat choices))
     )

     (= (count args) 4)
     (let [
           iD (first args)
           mD (second args)
           opt1 (nth args 2)
           opt2 (last args)
           choices (ml-pr (first opt1) (second opt1) (first opt2) (second opt2) prs)
           ]
       (cond

        (= choices nil) (list-alternate-diction rln prs iD mD)
        ;re do the search with a broader criteria
        :else (matchDic choices iD)
        )
       )
     )

   )



;; list of maps that worked
(def production-rule

  {
   'sameness
   {
    '(Q Q)
    {
     '(Q1 Q1) (list '( (ta ta) (te te) (tum tum) (. .)))
     '(Q1 Q2) (let [
                    a1 (combinations '(ta te tum .) 2)
                    a2 (map (partial reduce conj '()) a1 )
                    ]
                (list (concat a1 a2))
                )
     }
    '(EE EE)
    {

      '(E1E1 E1E1) (list '( ((ta ta) (ta ta)) ((te te) (te te)) ((tum tum) (tum tum)) ))
      '(E1E1 E1E2) (let [
                         a1 (combinations '(ta te tum .) 2)
                         a2 (map (partial reduce conj '()) a1 )
                         choices (list (concat a1 a2))
                         cp-choices (cartesian-product choices choices)
                         e1-choices   (map

                                       (fn [c1]

                                         (let [
                                               f (first c1)
                                               s (second c1)
                                               ]
                                           (cond

                                        ; E1E1
                                            (and (= (first f) (second f)) (not= (first s) (second f)))
                                            c1
                                            :else nil
                                            )
                                           )

                                         )
                                       cp-choices)
                         ]
                     (reduce concat e1-choices)
                     )

      '(E1E2 E1E2) (let [
                         a1 (combinations '(ta te tum .) 2)
                         a2 (map (partial reduce conj '()) a1 )
                         choices (list (concat a1 a2))
                         cp-choices (cartesian-product choices choices)
                         e1-choices   (map

                                       (fn [c1]

                                         (let [
                                               f (first c1)
                                               s (second c1)
                                               ]
                                           (cond

                                        ; E1E2
                                            (and (not= (first f) (second f)) (not= (first s) (second f)))
                                            c1
                                            :else nil
                                            )
                                           )

                                         )
                                       cp-choices)
                         ]
                     (reduce concat e1-choices)
                     )

      }

    }

   'difference
   {
    '(Q EE)
     {
      '(Q1 E1E1) (list '( (ta (ta ta)) (te (ta te)) (tum (tum tum)) ))
      '(Q1 E2E2) (list '( (ta (te te)) (ta (tum tum)) (te (ta ta)) (te (tum tum)) (tum (ta ta)) (tum (te te)) ))
      '(Q1 E1E2) (let [
                       a1 (combinations '(ta te tum .) 2)
                       a2 (map (partial reduce conj '()) a1 )
                       list-dh (concat a1 a2)
                       Qee    (map
                               (fn [d1]
                                 (map
                                  (fn [dh1]
                                    (cons d1 (list dh1))
                                    )
                                  list-dh)
                                 )
                               '(ta te tum))
                       ]
                   (map concat Qee)
                   )
      }

     '(EE Q)
      {
       '(E1E1 Q1) (list '( ((ta ta) ta) ((ta te) te) ((tum tum) tum) ))
       '(E2E2 Q1) (list '(((te te) ta) ((tum tum) ta) ((ta ta) te) ((tum tum) te) ((ta ta) tum) ((te te) tum)))
       '(E1E2 Q1) (let [
                        a1 (combinations '(ta te tum .) 2)
                        a2 (map (partial reduce conj '()) a1 )
                        list-dh (concat a1 a2)
                        Qee    (map
                                (fn [d1]
                                  (map
                                   (fn [dh1]
                                     (cons d1 (list dh1))
                                     )
                                   list-dh)
                                  )
                                '(ta te tum))
                        eeQ (map
                             (fn [d1]
                               (map (partial reduce conj '()) d1 ) )
                             Qee)
                        ]
                    (map concat eeQ)
                    )
       }
      }

   }

  )

(defprotocol q-pr
  ; 4 instantiations taking different parameters
  (query-pr
    [x relation] [x relation noteDuration] [x relation noteDuration literal] )
  )


(defrecord pr-rule [pr
                   ]
  q-pr
  (query-pr [this relation] (get (:pr this) relation))
  (query-pr [this relation noteDuration]  (get (get (:pr this) relation) noteDuration))
  (query-pr [this relation noteDuration literal] (get (get (get (:pr this) relation) noteDuration) literal) )

  )
