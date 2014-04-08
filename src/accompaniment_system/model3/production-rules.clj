;; Contains production rules that give the output choices in diction


;; map of maps that is used to display production rules
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

; Protocol that takes in different number arguments and returns appropriate results from the database

(defprotocol q-pr
  ; 4 instantiations taking different parameters
  (query-pr
    [x] [x relation] [x relation noteDuration] [x relation noteDuration literal] )
  )


(defrecord pr-rule [pr
                   ]
  q-pr
  (query-pr [this relation] (get (:pr this) relation))
  (query-pr [this relation noteDuration]  (get (get (:pr this) relation) noteDuration))
  (query-pr [this relation noteDuration literal] (get (get (get (:pr this) relation) noteDuration) literal) )
  (query-pr [this] (str "No match") )
  )


; The interface function that calls the query to the production rules
; The order of arguments are :
; Relation, note duration, literal and diction
(defn list-alternate-diction [& args]

  (cond
   (= (count args) 0) (query-pr (pr-rule. production-rule))
   (= (count args) 1) (do ;(println (first args))
                          (query-pr (pr-rule. production-rule) (first args))
                          )
   (= (count args) 2) (do ;(println (first args) (second args))
                          (query-pr (pr-rule. production-rule) (first args) (second args))
                          )
   (= (count args) 3) (do ;(println (first args) (second args) (last args))
                          (query-pr (pr-rule. production-rule) (first args) (second args) (last args))
                          )
   )
  )
