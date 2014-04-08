;; This file generates a transformation function from L1 and L2. L1 and L2 are described as workspace objects with certain representations. This contains the methods needed to obtain transformations between the workspace representations of L1 and L2.

;The general transformation is of the form "replace Beat no ____ by _____ ".
; Relational transformations:  "replace description-facet by relation
; Non relational transformations: "replace Beat no 4 by "ta, te, tum, Q1, (ta te)"

(defprotocol gen-rule
  (generate-rule [x replace-type relation] [x category2 descriptor2 facet2] )
                  ;relation rule           ;non relation rule
  )

(defrecord rule [category1 ; Abstr ND, note duration, literal, diction
                 descriptor1 ; BeatNo
                 descriptor1-facet
                 relation replace-type category2 descriptor2 descriptor2-facet
                 ]
  gen-rule
  (generate-rule [this r-type relation] (str "replace beat Number " (:descriptor1-facet this) " " (:descriptor1 this) " by " relation )  )
  (generate-rule [this c2 d2 f2] (str "replace beat Number " (:descriptor1 this) " by " d2 ) )

  )

(defn make-rule [r1 r2 r3 & args]
  (let [
        cnt (count args)
        rule-obj (cond

                  (= cnt 2) (rule. r1 r2 r3 (first args) (last args) nil nil nil)
                  (= cnt 3) (rule. r1 r2 r3 nil nil (first args) (second args) (last args) )

                  :else (do (println "Rule cannot be formed") nil)

                  )
        ]
    ;(println rule-obj)
    (cond

     (= cnt 2) ( generate-rule rule-obj (:relation rule-obj) (:replace-type rule-obj) )
     (= cnt 3) ( generate-rule rule-obj (:category2 rule-obj) (:descriptor2 rule-obj) (:descriptor2-facet rule-obj) )
     :else nil
     )
    )

    )

;category2 - Abstr ND, note duration, literal, diction
;descriptor ; BeatNo
;descriptor2-facet
;replace-type - Abstr ND, note duration, literal, diction
;relation - Sameness, difference(aND), double, half, 1/3rd (ND)


; As of now changed obj looks at 2 lists of diction, finds changed objects in the corresponding positions in the two lists and generates a non relational rule that connects both the objects


(defn eq [cur n]

  (cond

   (= cur n) cur
   (> n cur) n
   )
  )




;Checks for changes in bonds within the 2 strings given a description of the strings
; i/p 'noteduration (s d s) (d d s)
; Change noteduration description between 1st and 2nd positions to difference
; (bond/group-reason bond/group-reason bond/group-reason) to (bond/group-reason bond/group-reason bond/group-reason)


(defn compr [a1 a2]

  (cond

   (= a1 a2) 'sameness
   :else 'difference
   )

  )

(defn hit-relations [obj cnt]

  (cond

   (and (= cnt (dec (lengthList obj 0)) ) (or (= (charAtPos obj cnt 0) 'H) (= (charAtPos obj cnt 0) 'HQ) (= (charAtPos obj cnt 0) '(Q Q)) ) )
   (cons 'sameness (hit-relations obj (inc cnt)))
   ;; sameness - group reason
   (>= cnt (dec (lengthList obj 0))) nil
   :else (let [

               o1 (nth obj cnt)
               o2 (nth obj (inc cnt))
               ]
           (cond

            (or (= o1 'H) (= o1 '(Q Q)) ) (concat (list 'sameness (compr o1 o2)) (hit-relations obj (inc cnt))  )   ;; sameness - group reason

            (= o1 'HQ) (concat (list 'sameness 'sameness (compr o1 o2)) (hit-relations obj (inc cnt)))
            :else (cond

                   (= o1 o2) (cons 'sameness (hit-relations obj (inc cnt)))
                   :else (cons 'difference (hit-relations obj (inc cnt)))
                   )
            )
           )
   )
  )

(defn changed-obj-bonds [i-facet i-obj m-facet m-obj]

  (cond

   (= i-facet m-facet)  (let [
                              o1 (hit-relations i-obj 0)
                              o2 (hit-relations m-obj 0)
                              ]
                          (map

                           (fn [d1 d2]

                             (cond

                              (= d1 d2) 0
                              :else (make-rule d1 i-facet 'beatno i-facet d2)
                              )
                             )
                           o1 o2
                           )
                          )
   :else (println "Dissimilar facets of object description cannot be compared")
   )

  )

(defn symDur [s1]

      (cond
       (or (= s1 'Q) (= s1 '(Q Q))) 1/4
       (= s1 'H) 1/2
       (= s1 'EE) 1/8
       (or (= s1 'HQ) (= s1 'QH) (= s1 '(Q Q Q))) 3/4

       )

      )


(defn find-relation [s1 s2]

  (let [

        s1-val (symDur s1)
        s2-val (symDur s2)
        ;checking possible relations by note duration
        r1 (/ s2-val s1-val)
        r2 (/ s1-val s2-val)
        ]
    (cond

     (integer? r1) (list 'multiply r1)
     (integer? r2) (list 'multiply (/ 1 r2))
     :else nil
     )

    )

 )



; Contains a list of relations between note durations and the corresponding transformations that they result in.
; i/p 2 note durations, output -> multiplicative relation between them

(defn relation-note-durations [d1 d2]

  (let [

        s1 (cond

            (= (lengthList d1 0) 1) (first d1)
            :else d1
            )
        s2  (cond

             (= (lengthList d2 0) 1) (first d2)
             :else d2
             )
        ]
    (cond

     ;( and  (= s1 'Q) (= s2 'Q) )
     ;( and  (= s1 'EE) (= s2 'EE) )

     (= s1 s2) (list 'equal (symDur s1))
     (or

      ( and  (= s1 'Q) (= s2 'EE) )
      ( and  (= s1 'EE) (= s2 'Q) )
      ( and  (= s1 '(Q Q)) (= s2 'H) )
      ( and  (= s1 'H) (= s2 '(Q Q)) )
      )
     (find-relation s1 s2)

     ;Correct but not direct relation exists

     ( and  (= s1 '(Q EE)) (= s2 'H) ) 'nil ;No-direct-relation
     ( and  (= s1 'H) (= s2 '(Q EE)) ) 'nil ;No-direct-relation

     ;length mismatch cases that are returned back to the calling function to change length of one of the inputs

     (or

      ( and  (= s1 'Q) (= s2 'H) )
      ( and  (= s1 'H) (= s2 'Q) )
      ( and  (= s1 'EE) (= s2 'H) )
      ( and  (= s1 'H) (= s2 'EE) )
      ( and  (= s1 'EE) (= s2 '(Q Q)) )
      ( and  (= s1 '(Q Q)) (= s2 'EE) )
      ( and  (= s1 'HQ) (= s2 'H) )
      ( and  (= s1 'HQ) (= s2 'EE) )
      ( and  (= s1 'HQ) (= s2 'Q) )
      ( and  (= s1 'HQ) (= s2 '(Q Q)) )
      ( and  (= s2 'HQ) (= s1 'H) )
      ( and  (= s2 'HQ) (= s1 'EE) )
      ( and  (= s2 'HQ) (= s1 'Q) )
      ( and  (= s2 'HQ) (= s1 '(Q Q)) )

      ) (list 'length-mismatch (* 4 (- (symDur s2) (symDur s1)))  )

        )

    )

  )


; This is one of the protocol functions that takes in 2 strings, with their corresponding decriptors and makes respective relational rules for transformation.

(defn changed-obj [i-obj m-obj cur1 cur2 n1 n2]

  (cond

   (or (>= cur1 (lengthList i-obj 0)) (>= cur2 (lengthList m-obj 0))) nil

   :else

   (let [
         d1 (subsequ i-obj cur1 n1 0)
         d2 (subsequ m-obj cur2 n2 0)
         ]
     ;(println cur1 cur2 n1 n2 d1 d2 (lengthList i-obj 0))
     (cond


      (= d1 d2) (cons 0 (changed-obj i-obj m-obj (inc cur1) (inc cur2) (inc n1) (inc n2) ) )
      :else (let [
                  reln (relation-note-durations d1 d2)
                  ]
              (println d1 d2 (first reln) (last reln))
              (cond

               (not= nil reln)
               (cond

                (= (first reln) 'length-mismatch) (cond

                                        ; symbol of d1 > d2
                                                   (> 0 (last reln)) (changed-obj i-obj m-obj cur1 cur2 n1 (- n2 (last reln)))
                                        ; symbol of d1 < d2
                                                   (< 0 (last reln)) (do (println "here") (changed-obj i-obj m-obj cur1 cur2 (+ n1 (last reln)) n2)  )

                                                   )

                                        ;find a description starting at the same beat with an eq dur and find a relation
                :else (let [
                            cur1 (eq cur1 n1)
                            cur2 (eq cur2 n2)
                            ]
                        (cons (make-rule 'NoteDuration d1 cur1 'ND reln) (changed-obj i-obj m-obj (inc cur1) (inc cur2) (inc cur1) (inc cur2) ) )
                        )
                )
                                        ; Make a non relational rule
               :else (cons (make-rule 'diction cur1 d1 'diction (first d2) cur2) (changed-obj i-obj m-obj (inc cur1) (inc cur2) (inc cur1) (inc cur2) ) )
               )

              )
      )
     )

   )
  )



;; Right now it Assumes that only one object changes between o1 and o2.
(defn changed-obj-pos [obj1 obj2 pos]

  (let [
        o1 (charAtPos obj1 pos 0)
        o2 (charAtPos obj2 pos 0)
        ]
    (cond

     (>= pos (lengthList obj1 0)) -1 ; no object has changed
     (= o1 o2) (changed-obj-pos obj1 obj2 (+ pos 1))
     :else pos
     )
    )
  )
