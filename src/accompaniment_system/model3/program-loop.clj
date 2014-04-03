; Main program loop that takes in input strings L1, Modified string L2, Target string A1 and  and gives output A2.

(load-file "src/accompaniment_system/model3/utilities.clj")
(load-file "src/accompaniment_system/model3/transformations.clj")
(load-file "src/accompaniment_system/model3/accompaniment.clj")
(load-file "src/accompaniment_system/model3/production-rules.clj")

(defn main [l1 l2 a1]

  (let [
        l1R (rand-nth (clean-rep (all-representations l1)))
        l2R (rand-nth (clean-rep (all-representations l2)))

        l1R-D (ret-relevant-descriptions 'diction l1R)
        l2R-D (ret-relevant-descriptions 'diction l2R)

        l1R-ND (ret-relevant-descriptions 'noteDuration l1R)
        l2R-ND (ret-relevant-descriptions 'noteDuration l2R)

        l1R-l (ret-relevant-descriptions 'literal l1R)
        l2R-l (ret-relevant-descriptions 'literal l2R)

        Tr (changed-obj l1 l2 0 0 0 0)
        Tr2 (changed-obj l1R-ND l2R-ND 0 0 0 0)

        ;a2 (list-transforms Tr a1)

        a1R  (all-representations a1)
        ; find the description of a1R that matches l1R-ND
        ;If no description matches, then go to higher level of note duration relations

        check-match (reduce
                     concat
                     (map
                     (fn [a1r]

                      (let [
                            a1r-nd (ret-relevant-descriptions 'noteDuration a1r)
                            ]
                        (cond
                         (= a1r-nd l1R-ND) a1r-nd
                         :else nil
                         )
                        )
                      )
                     a1R
                     )
                     )

        ; searching the sapce for alternate diction possibilities
        alt-diction

        (cond

         (= check-match nil) ; no common note duration rep match
         ;sameness or difference between hits to find obj of change
         ;send alternate diction fnuuction which relation to change
         (let [
               reln-l1 (hit-relations l1R-ND)
               reln-l2 (hit-relations l2R-ND)
               pos (changed-obj-pos reln-l1 reln-l2 0)
               ; more generally this position can be between 2 non-adjacent hits as well
               d1 (nth l2 pos)
               d2 (nth l2 (+ pos 1))
               nd1  (nth l2R-ND pos)
               nd2 (nth l2R-ND (+ pos 1))
               lit1  (nth l2R-l pos)
               lit2 (nth l2R-l (+ pos 1))
               ]
           (list-alternate-diction (nth reln-l2) prod-rules)
           )

         :else ; atleast 1 matching representation exists in the note duration
         ;deciding that a particular transform exits in note duration
         (let [
               pos (changed-obj-pos l1R-ND l2R-ND 0)
               cpr (compr (nth l1R-ND pos) (nth l2R-ND pos) )
               ]
           (println "here")
           (cond

            (not= pos -1) (list-alternate-diction
                           cpr
                           prod-rules
                           (nth l1 pos) (nth l2 pos)
                           (list (nth l1R-ND pos) (nth l2R-ND pos))
                           (list (nth l1R-l pos) (nth l2R-l pos))
                           )
            :else (let [
                        pos (rand-int 3)
                        cpr (compr (nth l1R-ND pos) (nth l2R-ND pos) )
                        ]
                    (println "same diction, pos selected at random")
                    (list-alternate-diction
                     cpr
                     prod-rules
                     (nth l1 pos) (nth l2 pos)
                     (list (nth l1R-ND pos) (nth l2R-ND pos))
                     (list (nth l1R-l pos) (nth l2R-l pos))
                     )
                    )
            )
           )



         )

        ;a2R1 (ret-relevant-descriptions 'noteDuration a2R)
        ;a2nd  (list-transforms Tr2 a2R1)
        ]
    ;(println a2)
    ;(println "choices are:")
    alt-diction
    )
  )


(defn clean-rep [lists]

  (cond

   (empty? lists) nil
   (= nil (first lists))  (clean-rep (rest lists))
   :else (cons (first lists) (clean-rep (rest lists)))
   )

  )
