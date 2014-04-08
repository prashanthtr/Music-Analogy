; Main program loop that takes in input strings L1, Modified string L2, Target string A1 and  and gives output A2.

(load-file "src/accompaniment_system/model3/utilities.clj")
(load-file "src/accompaniment_system/model3/transformations.clj")
(load-file "src/accompaniment_system/model3/accompaniment.clj")
(load-file "src/accompaniment_system/model3/production-rules.clj")
(load-file "src/accompaniment_system/model3/workspace-objects.clj")
(load-file "src/accompaniment_system/model3/visualizations.clj")
(load-file "src/accompaniment_system/model3/sound.clj")


(defn subst-choices [choices diction pos]

  (cond
   (empty? choices) nil
   :else (let [
               f (first choices)
               subst (second f)
               arr (assoc diction pos subst)
               ]
           ;(println "first" f "second" subst)
           (cons arr (subst-choices (rest choices) diction pos))
           )
   )
  )


(defn subst-choices2 [choices diction pos1 pos2]

  (cond
   (empty? choices) nil
   :else (let [
               f (first choices)
               subst1 (first f)
               subst2 (second f)
               arr (assoc diction pos1 subst1)
               arr (assoc diction pos2 subst2)
               ]
           ;(println "first" f "second" subst)
           (cons arr (subst-choices2 (rest choices) diction pos1 pos2))
           )
   )
  )




(defn main [l1 l2 a1]

  (let [
        l1R (rand-nth (descriptions l1))
        l2R (rand-nth (descriptions l2))

        l1R-D (ret-descriptions l1R 'diction )
        l2R-D (ret-descriptions l2R 'diction )

        l1R-ND (ret-descriptions l1R 'noteDuration )
        l2R-ND (ret-descriptions l2R 'noteDuration)

        l1R-l (ret-descriptions l1R 'literal )
        l2R-l (ret-descriptions l2R 'literal)

        a1R  (descriptions a1)

        ; find the description of a1R that matches l1R-ND
                                        ;If no description matches, then go to higher level of note duration relations
        check-match (reduce
                     concat
                     (map
                      (fn [a1r]

                        (let [
                              a1r-nd (ret-descriptions a1r 'noteDuration)
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

        ;searching the sppce for alternate diction possibilities
        alt-diction

        (cond

         (empty? check-match)
         ;no common note duration rep match
         ;sameness or difference between hits to find obj of change
         ;send alternate diction fnuuction which relation to change
         (let [
               reln-l1 (hit-relations l1R-ND 0)
               reln-l2 (hit-relations l2R-ND 0)
               pos (changed-obj-pos reln-l1 reln-l2 0)
               ;more generally this position can be between 2 non-adjacent hits as well
               d1 (nth l2 pos)
               d2 (nth l2 (+ pos 1))
               nd1  (nth l2R-ND pos)
               nd2 (nth l2R-ND (+ pos 1))
               l1  (nth l2R-l pos)
               l2 (nth l2R-l (+ pos 1))
               tar-reln (nth reln-l2 pos)
               src-reln (nth reln-l1 pos)
               choices
               (concat
                (list-alternate-diction tar-reln (list nd1 nd1) (list l1 l1) )
                (list-alternate-diction tar-reln (list nd2 nd2) (list l2 l2) )
                (list-alternate-diction tar-reln (list nd1 nd2) (list l1 l2) )
                (list-alternate-diction tar-reln (list nd2 nd1) (list l2 l1) )
                )
               ]
           ;(println "check match" check-match)
           (println "The production rule is" (make-rule 'relation src-reln pos 'relation tar-reln) )

           (println "The choices are: " choices)
           ;; need further mechanisms to select from amongst the choices specified by the abstract description
           (subst-choices2 (reduce concat choices) (into [] a1) pos (+ pos 1))
           )

         :else ; atleast 1 matching representation exists in the note duration
                                        ;deciding that a particular transform exits in note duration
         (let [
               pos (changed-obj-pos l1R-ND l2R-ND 0)
               cpr (compr (nth l1R-ND pos) (nth l2R-ND pos) )
               ]
           ;(println "check match" check-match)
           (cond

            ;Relation, note duration, literal and diction
            (not= pos -1) (let [
                                choices (list-alternate-diction
                                         cpr
                                         (list (nth l1R-ND pos) (nth l2R-ND pos))
                                         (list (nth l1R-l pos) (nth l2R-l pos))
                                         )
                                ]
                            (println "Choices are" choices)
                            (subst-choices (first choices) (into [] a1) pos)
                            )

            :else (let [
                        pos (rand-int 3)
                        cpr (compr (nth l1R-ND pos) (nth l2R-ND pos) )
                        choices (list-alternate-diction
                                 cpr
                                 (list (nth l1R-ND pos) (nth l2R-ND pos))
                                 (list (nth l1R-l pos) (nth l2R-l pos))
                                 )
                        ]
                    (println "same diction, pos selected at random")
                    (println "Choices are" choices)
                    (subst-choices (first choices) (into [] a1) pos)
                    )
            )
           )
         )
        ]
    ;(println l1R-ND)
    ;(println l2R-ND)
    {'l1 l1R 'l2 l2R 'a1 {'diction a1 'noteDuration check-match} 'a2 alt-diction}
    )
  )

;; Get descriptions
;; connect it to the program-loop
;; link to sound

;--------------------------

; Mechanism to say which are good and bad with lead
;; Lead without melody for now
;; This is done to figure out the use of context -
;; Lead with melody to say how much far in the past to go.
