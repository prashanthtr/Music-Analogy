;; Creates visualizations of the objects in the works


;;Input that needs to be updated in the diagram

                                        ;({bond-type sameness, bond-value ta, duration 1/2, obj1 {beatNo 1, diction ta, literal Q1, duration 1/4}, obj2 {beatNo 2, diction ta, literal Q1, duration 1/4}} {bond-type sameness, bond-value tum, duration 1/2, obj1 {beatNo 3, diction tum, literal Q2, duration 1/4}, obj2 {beatNo 4, diction tum, literal Q2, duration 1/4}})

(load-file "src/accompaniment_system/model3/workspace-objects.clj")

(defn tree [ip]

  (let [

        list-ip   (map

                   (fn [d]

                     (cond

                      (not= (get d 'beatNo) nil)  [ (get d 'noteDuration) [ (get d 'literal)  [(get d 'diction) [(get d 'beatNo)] ]]
                       ]
                      :else
                      [ (get d 'noteDuration)
                        [ (get (get d 'obj1) 'literal) [ (get (get d 'obj1) 'diction) [(get (get d 'obj1) 'beatNo)]  ] ]
                        [ (get (get d 'obj2) 'literal) [ (get (get d 'obj2) 'diction) [(get (get d 'obj2) 'beatNo)]  ] ]
                        ]
                      )
                     )
                   ip)
        ]
    (into [] list-ip)

    )
  )


(defn draw-l1-l2 [l1 l2 tl1 tl2]

  (let [
        treel1 (tree l1)
        treel2 (tree l2)
        ]
    ;(println treel1)
    (vijual/draw-tree [(into [] (concat [tl1] treel1)) (into [] (concat [tl2] treel2))])
    )
  )

;; Extensibility of tree to higher levels of abstraction


(defn gen-and-draw [diction1 diction2]

  (let [
        l1-rep (all-representations diction1)
        l2-rep (all-representations diction2)
        some-l1  (rand-nth (rest l1-rep))
        some-l2 (rand-nth (rest l2-rep))
        ]
    (draw-l1-l2 some-l1 some-l2)
    )

  )

;[[Q [Q1 [ta [1]]]] [Q [Q2 [tum [2]]]] [Q [Q2 [tum [3]]]] [Q [Q1 [ta [4]]]]]

(defn consinto [a1 a2]

  (concat a1 (into [] a2))

  )







(comment

;; musically characterize the space
;;
(defmulti query-prod-rules
  (fn [prs x]
     (get prs x)
     )
  )

(defmethod query-prod-rules 'sameness [pr x & args]
  (get pr 'content)
  )

(defmethod query-prod-rules 'difference [pr x & args]
   (get pr 'content)
   )

(defmethod query-prod-rules 'noteDuration [pr x & args]
   (get pr 'content)
   )

(defmethod query-prod-rules 'literal [pr x & args]
   (get pr 'content)
   )

;descriptions
(defmethod query-prod-rules '(relation noteDuration) [pr x & args]
  (get ( query-prod-rules (query-prod-rules pr 'relation) 'noteDuration) x)
  )


(defmethod query-prod-rules (query-prod-rules pr 'relation)

  [pr x & args]
  (get (query-prod-rules pr 'content) x )
  )

)

;; query -> ta te Q1 Q2 Q Q sameness
;; ta (ta te) Q1 E1E2 Q EE difference
; or arguments in the reverse order

;query prod rules take from 1 arg to 4 args



(comment

  (def rule-tree (let [
                       top (first (into [] pr))
                     i1 (reduce conj (map (partial into []) (first top) ) )
                     i2 (reduce conj (map (partial into []) (last top)) )
                     i3 (reduce conj (map (partial into []) (last i1)) )
                     i4 (reduce conj (map (partial into []) (last i2)) )
                     leaf (reduce conj (map (partial into []) (last i3)) )
                     ]
                 (println top)
                 (println i1)
                 (println i2)
                 (println i3)
                 (println leaf)
                                        ;(println (consinto (list (first top)) (first i2)))

                 (into []
                       (concat
                        [ (second top) ]
                        [
                         (into []
                               (concat
                                [  (list (first i3) (second i3)) ]
                                (into [] [leaf])
                                )
                               )

                         ])
                       )

                 )
    )

  )
