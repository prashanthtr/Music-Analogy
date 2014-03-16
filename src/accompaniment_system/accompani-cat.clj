(defn accompani-act [])

;;Utlities

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

(defn obj-measure [number diction loudness category-type obj-category]

  (let [

        link {'m-no number}
        link (merge link {'diction diction})
        link (merge link {'loudness loudness})
        link (merge link {'obj-category obj-category}) ; measure, group
        link (merge link {'category-type category-type}) ;Category type - raw input, workspace structure
        ]
    link
    )
  )

(defn contour-map-fn [x1 x2]

  (let [
        dif (- x2 x1)
        ]
    (cond

     (= 'dif 0) '-
     (and (<= dif -0.25) (> dif -0.5)) 'd
     (and (>= dif 0.25) (< dif 0.5)) 'u
     (or (and (<= dif 0) (> dif -0.25)) (and (>= dif 0) (< dif 0.25))) '-
     (>= dif 0.5) 'U
     (<= dif -0.5) 'D
     )
    )
  )

(defn build-loudness-contour [loudness st]

  (cond
   (>= st (lengthList loudness 0)) nil
                                        ;   (= st 0) (cons (contour-map-fn 0.75 (charAtPos loudness 0 0)) (build-loudness-contour loudness (+ st 1)))
   (= st 0) (build-loudness-contour loudness (+ st 1))
   :else (cons (contour-map-fn (charAtPos loudness (- st 1) 0) (charAtPos loudness st 0)) (build-loudness-contour loudness (+ st 1)))

   )

  )

(defn loudness-contour [obj-measure]

  {'loudness-contour (build-loudness-contour (get obj-measure 'loudness) 0)}

  )


;; returns the number of attacks at each beat position,
(defn attacks-at-pos [rhythm]

  (cond
   (empty? rhythm) nil
   (list? (first rhythm)) (cons 2 (attacks-at-pos (rest rhythm) ))
   (= '. (first rhythm)) (cons 0 (attacks-at-pos (rest rhythm) ))
   :else (cons 1 (attacks-at-pos (rest rhythm)))

   )

  )

(defn rhythm-contour [obj-measure]

  {'rhythm-contour (attacks-at-pos (get obj-measure 'diction))}

  )

(defn add-workspace-descriptions [obj-measure]

  (cond

   (= (get obj-measure 'category-type) 'workspace-obj)
   (merge obj-measure (loudness-contour obj-measure) (rhythm-contour obj-measure)  )

   :else obj-measure
   )

  )


(defn positive [x]

  (cond

   (< x 0) (- 0 x)
   :else x
   )

  )

(defn disp  [measure-link]

  (cond

   (empty? measure-link) nil
   :else (do (println (first measure-link)) (disp (rest measure-link)))

   )

  )

; Takes 2 workspace-obj measures and finds link using attacks
(defn measure-linker [m1 m2]

  (let [

        attack1 (get m1 'rhythm-contour)
        attack2 (get m2 'rhythm-contour)
        dif (reduce + (map positive (map - attack1 attack2)))
        sum (reduce + (map + attack1 attack2))
        similarity (float  (* 100 (- 1 (/ dif sum))))
        measure-link (merge {'obj1 (into [] m1)} {'obj2 (into [] m1)} {'similarity similarity}   {'obj-category 'measure-linker} {'category-type 'workspace-obj}  )
        ]
    measure-link
    ;(disp measure-link)
    )

  )

(defn contour-match[lc1 lc2 st]

  (let [
        h1 (charAtPos lc1 st 0)
        h2 (charAtPos lc2 st 0)
        ]
    (cond
     (>= st (lengthList lc1 0)) nil
     (= h1 h2) (cons 0 (contour-match lc1 lc2 (+ st 1)))
     (or (= (str h1) (lower-case h2)) (= (str h2) (lower-case h1))) (cons 0 (contour-match lc1 lc2 (+ st 1)))
     :else (cons 1 (contour-match lc1 lc2 (+ st 1)) )
     )
    )

  )




(defn relaxed-contour-match [lc1 lc2]

  (let [
        match-arr (contour-match lc1 lc2 0)
        match (reduce + match-arr)
        ]
    (println match-arr)
    (cond

     (= match 0) 'identical
     :else 'similar

     )
    )

  )


(defn loudness-contour-linker [m1 m2]

  (let [
        contour1 (get m1 'loudness-contour)
        contour2 (get m2 'loudness-contour)
        descriptor (cond

                    (= contour1 contour2) 'identical
                    :else 'similar
                    )
        descriptor (if
                    (= descriptor 'similar) (relaxed-contour-match contour1 contour2)
                    descriptor
                    )
        similarity (cond

                    (= descriptor 'identical) 100.0
                    :else 50.0
                    )

        ]
    (merge {'obj1 (into [] m1)} {'obj2 (into [] m1)} {'obj-category 'loudness-contour} {'similarity similarity} {'description descriptor})
    )
  )

(defn partit [list]

  (partition (/ (lengthList list 0) 2) list)

  )

(defn array-category-type [object category-type]

  (get object category-type)

  )

;;generic realatinship builder module
(defn build-relatonship[type part-of-pattern description obj1 obj2]



  )

;; takes in 2 measures and checks the ground on which relationship can be formed based on measure links, should be extended to loudness and note activity
(defn relationship-check [m1 m2 similarity-fn measure-facet]

  (let [
        similarity (get (similarity-fn m1 m2) 'similarity)
        description (cond

                     (= similarity 100.0) 'identical
                     :else 'similar
                     )
        part-of-pattern 'whole
        type measure-facet
        desc-whole description

        change-desc (cond

                     (not= description 'identical)

                     (let [


                           first-diction-m1  {'diction (first (partit (array-category-type m1 'diction)) )}
                           first-loudness-m1  {'loudness (first (partit (array-category-type m1 'loudness)) )}
                           half-m1  (merge m1 first-diction-m1 first-loudness-m1)
                           first-diction-m2  {'diction (first (partit (array-category-type m2 'diction)) )}
                           first-loudness-m2  {'loudness (first (partit (array-category-type m2 'loudness)) )}
                           half-m2  (merge m2 first-diction-m2 first-loudness-m2)
                           half-m1 (add-workspace-descriptions half-m1)
                           half-m2 (add-workspace-descriptions half-m2)
                           half-measure (get (similarity-fn half-m1 half-m2) 'similarity
                                             )
                           ]
                       (cond

                        (= half-measure 100.0) 'identical
                        :else 'similar
                        )
                       )
                     :else nil
                     )

        cd1 change-desc
        part-of-pattern (cond

                         (and (not= description 'identical) (= change-desc 'identical)) 'first
                         :else part-of-pattern
                         )

        description (cond

                     (= part-of-pattern 'first) 'identical
                     :else description

                     )

        change-desc (cond
                     (not= description 'identical)
                     (let [


                           first-diction-m1  {'diction (last (partit (array-category-type m1 'diction)) )}
                           first-loudness-m1  {'loudness (last (partit (array-category-type m1 'loudness)) )}
                           half-m1  (merge m1 first-diction-m1 first-loudness-m1)
                           first-diction-m2  {'diction (last (partit (array-category-type m2 'diction)) )}
                           first-loudness-m2  {'loudness (last (partit (array-category-type m2 'loudness)) )}
                           half-m2  (merge m2 first-diction-m2 first-loudness-m2)
                           half-m1 (add-workspace-descriptions half-m1)
                           half-m2 (add-workspace-descriptions half-m2)
                           half-measure (get (similarity-fn half-m1 half-m2) 'similarity
                                             )
                           ]
                       (cond

                        (= half-measure 100.0) 'identical
                        :else 'similar
                        )
                       )
                     :else nil
                     )
        part-of-pattern (cond

                         (and (not= description 'identical) (= change-desc 'identical)) 'last
                         :else part-of-pattern
                         )
        description (cond

                     (= part-of-pattern 'last) 'identical
                     :else description

                     )


        ;loudness-contour
        ;note-activity
        ]
    (merge

     {'type type} ;rhythm-contour, loudness, note activity, analogy
     {'part-of-pattern part-of-pattern} ;start, end, whole for identifying parts that are identical
     {'description description} ; For the first 3, similar and identical, for analogy - analogoues
     {'obj1 (into [] m1)} {'obj2 (into [] m1)}
     )
    )

  )


(defn find-changed-description [src modi similarity-fn type]

  (let [
        reln (relationship-check src modi similarity-fn type)
        description (get reln 'description)
        part (get reln 'part-of-pattern)
        ]
    (cond

     (= description 'identical) (cond

                                 (= part 'whole) nil
                                 (= part 'first) 'last
                                 (= part 'last) 'first
                                 )
     (= description 'similar) 'not-sure
     )

    )

  )

; take relationship and infer rule
(defn changed-obj [src modi]

  (let [

        len  (lengthList (get src 'diction) 0)
        obj-changed (cond

                    (> len 4)
                    (let [

                          description (find-changed-description src modi measure-linker 'rhythm-contour)
                          type 'rhythm-contour

                          ;;changed object when len of measure > 4
                          obj-changed (cond

                                  (= description 'not-sure) nil
                                  (= description nil) (merge src {'change-facet type} {'change 'whole} )
                                  (= description 'last)

                                  (let [

                                        first-diction-m1  {'diction (last (partit (array-category-type modi 'diction)) )}
                                        first-loudness-m1  {'loudness (last (partit (array-category-type modi 'loudness)) )}
                                        half-m1  (merge src first-diction-m1 first-loudness-m1)
                                        half-m1 (add-workspace-descriptions half-m1)

                                        ]
                                    (merge half-m1 {'change 'last} {'change-facet type})
                                    )
                                  (= description 'first)
                                  (let [
                                        first-diction-m1  {'diction (first (partit (array-category-type modi 'diction)) )}
                                        first-loudness-m1  {'loudness (first (partit (array-category-type modi 'loudness)) )}
                                        half-m1 (merge src first-diction-m1 first-loudness-m1)
                                        half-m1 (add-workspace-descriptions half-m1)
                                        ]
                                    (println "rhythm contour and first time 2" type half-m1)
                                    (merge half-m1 {'change 'first} {'change-facet type})
                                    )

                                  )
                          obj-changed (cond

                                        ;change description-type to loudness when the curent descriptor does not work
                                       (or  (= (get obj-changed 'change) 'whole) (= nil obj-changed))
                                       (let [
                                             description (find-changed-description src modi loudness-contour-linker 'loudness-contour)
                                             type 'loudness-contour
                                             ]
                                         (cond

                                          (= description 'not-sure) nil
                                          (= description nil) (merge obj-changed {'change 'whole} {'change-facet 'All}) ;only for one of the facets
                                          (= description 'last)

                                          (let [
                                                first-diction-m1  {'diction (last (partit (array-category-type modi 'diction)) )}
                                                first-loudness-m1  {'loudness (last (partit (array-category-type modi 'loudness)) )}
                                                a (do (println "modi " modi) 1)
                                                half-m1  (merge modi first-diction-m1 first-loudness-m1)
                                                half-m1 (add-workspace-descriptions half-m1)
                                                ]
                                            (println "rhythm contour and first time 3" type half-m1)
                                            (merge half-m1 {'change 'last} {'change-facet type})
                                            )
                                          (= description 'first)
                                          (let [
                                                first-diction-m1  {'diction (first (partit (array-category-type modi 'diction)) )}
                                                first-loudness-m1 {'loudness (first (partit (array-category-type modi 'loudness)) )}
                                                half-m1  (merge src first-diction-m1 first-loudness-m1)
                                                half-m1 (add-workspace-descriptions half-m1)
                                                ]
                                            (println "rhythm contour and first time 4" type half-m1)
                                            (merge half-m1 {'change 'first} {'change-facet type})
                                            )
                                          )

                                         )
                                  :else  obj-changed
                                        ;make it probablistically look for another descriptor and change, but not needed at the moment
                                  )
                          ]
                      obj-changed
                      )


                    :else  (let [

                                 d-rhythm (find-changed-description src modi measure-linker 'rhythm-contour)
                                 d-loudness (find-changed-description src modi loudness-contour-linker 'loudness-contour)

                                 ]

                             (cond

                              (and (= d-rhythm 'not-sure) (= d-loudness 'not-sure)) nil ;no identicality
                              (or (= d-rhythm 'not-sure) (= d-loudness nil)) (merge modi {'change-facet 'rhythm-contour} {'change 'whole} ) ;; MErge the rhyhtm contour fully
                              (or (= d-rhythm nil) (= d-loudness 'not-sure)) (merge modi {'change-facet 'loudness-contour} {'change 'whole} ) ;; MErge the loudness contour fully
                              (and (= d-rhythm nil) (= d-loudness nil)) (merge modi {'change-facet 'All} {'change 'whole} )  ;Both dimensions identicality

                              )
                             )
                    )
        ]
    (println "obj changed" obj-changed)
    obj-changed  ;almost like the rule that has to do transformation
    )

  )


(defn copy-properties [changed-obj target-obj]

  (let [
        part (get changed-obj 'change)
        ]

    (cond

     (= part 'whole) (do (println "full transformation") (merge target-obj changed-obj)) ; directly map changed to target
     :else (let [

                 diction (partit (array-category-type target-obj 'diction))
                 loudness (partit (array-category-type target-obj 'loudness))
                 diction (cond

                          (= part 'first) (concat (get changed-obj 'diction) (last diction))
                          (= part 'last) (concat (first diction) (get changed-obj 'diction)  )
                          )
                 loudness (cond

                          (= part 'first) (concat (get changed-obj 'loudness)  (last loudness))
                          (= part 'last) (concat (first loudness) (get changed-obj 'loudness)  )
                          )
                 obj-category 'measure
                 category-type 'workspace-obj
                 measure-number (+ 1 (get target-obj 'm-no))
                 ]
             (add-workspace-descriptions (obj-measure measure-number  diction loudness category-type obj-category))
             )
     )
    )

  )

; apply rule to first measure of secondary to change


; object of change to be identified in the source string (measure of 8 beats) based on comparison of loudness contour, and if loudness contour is the change from the first string, it has to be applied to the correct position in the target

; At the current moment all of it is only measures
(defn accompani-cat [source modified]

  (let [
        target-string (get source 'diction)

        ;create source, modified and target in workspace and add workspace description

        source-obj (obj-measure 1 (get source 'diction) (get source 'loudness) 'workspace-obj 'measure)
        source-obj (add-workspace-descriptions source-obj)
        modified-obj (obj-measure 2 (get modified 'diction) (get modified 'loudness) 'workspace-obj 'measure)
        modified-obj (add-workspace-descriptions modified-obj)
        target-obj (obj-measure 2 (get target-string 'diction) (get target-string 'loudness) 'workspace-obj 'measure)
        target-obj (add-workspace-descriptions target-obj)

        change-obj (changed-obj source-obj modified-obj ) ;Descriptions of the source that have changed in the modified object.
        answer (copy-properties change-obj target-obj)
        ]
    (println "changed object")
    (disp change-obj)
    (println "answer")
    (disp answer)   ; answer by making the analogy
    )
  )

;;; a : b
;;; c : d??


(defn pc-constraint [pc]

  (map (fn [x]

        (cond
         (= x 1) 'tum
         :else 'var
         )

        )
       pc
       )
  )


(defn lc-constraint [lc]

  (map (fn [x]

        (cond
         (= x 0) '.
         :else 'var
         )

        )
       lc
       )
  )


(defn rc-constraint [rc]

  (map (fn [x]

        (cond
         (not= x 0) '(ta te)
         :else 'var
         )

        )
       rc
       )
  )


(defn ret-decided-el[list]

    (map  (fn [r]

      (cond
       (not= r 'var) r
       :else 'var
       )

      ) list )

  )


  (defn orthogonal-transformations [tar]

    (let [
          loudness (get tar 'loudness)
          NA (get tar 'NA)
          pitch-bend (get tar 'pitch-bend)
          final (map + loudness NA pitch-bend)

          f-loud (reduce + (map - loudness final))
          f-NA (reduce + (map - NA final))
          f-pitch (reduce + (map - pitch-bend final))
          ]

      (println final (map - loudness final))
      (cond

       (and (< f-loud 0) (< f-NA 0) (< f-pitch 0) ) 'orthogonal
       :else nil
       )
      )
    )


;; takes in the abstract representations and generates diction
(defn generate-diction [tar]

  (let [

        loudness (get tar 'loudness)
        NA (get tar 'NA)
        pitch-bend (get tar 'pitch-bend)
        diction (get tar 'diction)

        r1 (lc-constraint loudness) ;represenations to merge
        r2 (rc-constraint NA)
        r3 (pc-constraint pitch-bend)

        lc (ret-decided-el r1)
        rc  (ret-decided-el r2)
        pc (ret-decided-el r3)

        output (orthogonal-transformations tar)
        ]

    (cond

     (= output 'orthogonal) (map (fn [l r p d]

                                   (cond

                                    (not= l 'var) l
                                    (not= r 'var) r
                                    (not= p 'var) p
                                    :else d
                                    )

                                   )
                                 loudness NA pitch-bend diction
                                 )
     :else nil
     )

    )
  )

;; Test case

;{'rep 'source 1 'loudness '(1 0.5 0.5 0.5) 'NA '(0 2 0 0) 'pitch-bend '(1 0 0 0) 'diction '(ta tum tum ta)}
;{'rep 'transformation 'loudness '(1 0.5 0.5 0.5) 'NA '(2 2 0 0) 'pitch-bend '(0 0 0 1) 'diction '(ta tum tum ta)}

;; At the moment it is random, but it will be a weighted sum of previous measures

(defn decide-representations [prev-measure transformation]

  (let [
        tar (rand-nth (list prev-measure transformation))
        ]
    (println (get tar 'rep))
    (generate-diction transformation)
    )

  )

(defn list-rev-list [soln el]

  (clojure.core/reverse (conj (clojure.core/reverse soln) el))

  )

;appends a set of lists with common element and returns set
(defn append-lists [lists el]

  (cond

   (= (first lists) nil) nil
   (= (first lists) '()) (cons (list-rev-list (first lists) el) (append-lists (rest lists) el))

   :else (cons (list-rev-list (first lists) el) (append-lists (rest lists) el))
   )

  )

(defn create-rc-lc [lists rc lc]

  (list (list-rev-list lists rc) (list-rev-list lists lc) )

  )


(defn expand-soln-space [soln rc lc]

  (cond

   (empty? soln) nil
   :else (concat (create-rc-lc (first soln) rc lc) (expand-soln-space (rest soln) rc lc))
   )

  )


; if transformation is not orthogonal

(defn lc-rc-orth [lc rc pc diction soln]

  (cond

   (or (empty? lc) (empty? rc) (empty? pc)) soln

   (and (= (first lc) 'var) (= (first rc) 'var) (= (first pc) 'var))
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction) ;(append-lists soln 'var)
               (expand-soln-space soln (first diction) (rand-nth '(ta tum)))
               )

   (and (= (first lc) 'var) (= (first rc) 'var) (not= (first pc) 'var))
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction) (append-lists soln (first pc)))

   (and (= (first lc) 'var) (not= (first rc) 'var) (= (first pc) 'var))
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction) (append-lists soln (first rc)))

   (and (not= (first lc) 'var) (= (first rc) 'var) (= (first pc) 'var))
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction) (append-lists soln (first lc)))

   (and (not= (first lc) 'var) (not= (first rc) 'var) (= (first pc) 'var) )
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction)  (expand-soln-space soln (first rc) (first lc))
   )

   (and (not= (first lc) 'var) (= (first rc) 'var) (not= (first pc) 'var) )
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction)  (expand-soln-space soln (first lc) (first pc))
   )

   (and (= (first lc) 'var) (not= (first rc) 'var) (not= (first pc) 'var) )
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction)  (expand-soln-space soln (first rc) (first pc))
   )

   (and (not= (first lc) 'var) (not= (first rc) 'var) (not= (first pc) 'var) )
   (lc-rc-orth (rest lc) (rest rc) (rest pc) (rest diction) (append-lists soln (first diction) )
   )

   )
  )

(defn gen-soln-space [loudness NA pitch-bend diction]

  (let [

        r1 (lc-constraint loudness)
        r2 (rc-constraint NA)
        r3 (pc-constraint pitch-bend)

        sol1 (lc-rc-orth r1 r2 r3 diction '(()))
        ]
    sol1
    )

  )



;; forms an internal representation of the rhythm pattern consisting of- same hits, different hits

(defn diction-contour [diction]

  (map (fn [d1 d2]

         (cond

          (= d1 d2) {'s 2 'hit d1}
          :else 'd
          )

         )
       diction (rest diction)
       )
  )

(defn dc-build [dc]

    (map (fn [d1 d2]

         (cond

          (and (= (get d1 's) (get d2 's)) (not= d1 'd)) (merge d1 {'s (+ (get d1 's) 1)})
          :else 'd
          )

         )
       dc (rest dc)
       )

  )

(defn has-collection [lists]

  (cond

   (empty? lists) nil
   (coll? (first lists)) 1
   :else (has-collection (rest lists))

   )

  )

(defn recur-dc [dc level]

  (let [
        cur-dc dc
        next-dc (dc-build cur-dc)
        ]
    (cond

     (= 1 (has-collection next-dc)) (cons {'level level 'desc cur-dc} (recur-dc next-dc (+ level 1)) )
     :else (list {'level level 'desc cur-dc})

     )

    )

  )


(defn NA-build [diction]

    (map (fn [d1 d2]

         (cond

          (and (coll? d1) (coll? d2)) {'s 2}
          :else 'd
          )

         )
       diction (rest diction)
       )

    )

;; Codelet from diction to generating description of possible pitch bend locations

(defn pitch-bend-loudness [diction]

    (map (fn [d1 d2]

         (cond

          (and (not= d1 '.) (= '. d2)) {'s 2 'hit d1}
          (and (= '. d1) (= '. d2)) {'s 2 'hit '.}
          :else 'd
          )

         )
       diction (rest diction)
       )
    )

;; Diction contour to possible pitch bend locations (similarity of successive hhtis)

(defn pitch-bend-dc [dc]

  (let [
        d (first dc)
        ]
    (cond

     (empty? dc) nil
     (and (not= d 'd) (= (get d 'hit) 'tum)) (concat '(1 2) (pitch-bend-dc (rest dc)))
     (and (not= d 'd) (not= (get d 'hit) 'tum)) (concat '(0 0) (pitch-bend-dc (rest dc))) ; has to be change in the loudness
     :else (cons 0 (pitch-bend-dc (rest dc)))
     )

    )
  )


;; part of the code that gets pitch-bend description to values
;; generate values from pitch bend

(defn numb [numbr]

  (cond

   (= numbr 2) '(1 0)
   (= numbr 3) '(2 0 0)
   (= numbr 3) '(3 0 0 0)
   )
  )


(defn zero-num [numbr]

  (cond

   (= numbr 2) '(0 0)
   (= numbr 3) '(0 0 0)
   (= numbr 4) '(0 0 0 0)
   )
  )


(defn pitch-bend-loud [dc]

  (let [
        d (first dc)
        ]
    (cond

     (empty? dc) nil
     (and (not= d 'd) (= (get d 'hit) 'tum)) (concat (numb (get d 's)) (pitch-bend-dc (rest dc)))
     (and (not= d 'd) (not= (get d 'hit) 'tum)) (concat (zero-num (get d 's)) (pitch-bend-dc (rest dc)))
     :else (cons 0 (pitch-bend-dc (rest dc)))
     )

    )
  )



(defn repr2acc-l1 [D NA]

  (cons

   ;first beat
   (cond

    (and (=  (get (first NA) 'd) 'p) (= (get (first D) 's) 2)  ) '.
    (and (=  (get (first NA) 'd) 'p) (not= (get (first D) 's) 2)  ) 'tum
    :else '?X
    )
   ;rest of beats
   (map
    (fn [d1 d2]

      (cond

       (= (get d2 'd) 'p) '.
       (= (get d2 'd) 's) '?X
       (= (get d2 'd) 'D) '(?X)
       )

      )
    D NA)

   )

  )

(defn single-subs []

  '(ta te tum)

  )

(defn double-subs []

  '((ta te) (ta tum) (ta ta) (te te) (te ta) (tum ta) (tum tum))

  )

(defn repr2acc-l2 [D NA]

  (let [
        repr (repr2acc-l1 D NA)
        ]

    (concat

     (map

      (fn [d1 d2]

        (cond

         (and (= d1 '?X) (= d2 '.)) 'tum
         (and (= d1 '(?X)) (= d2 '.)) '( (ta tum) (tum tum) )
         (and (= d1 '?X) (not= d2 '.)) (single-subs)
         (and (= d1 '(?X)) (not= d2 '.)) (double-subs)
         :else d1
         )

        )
      repr (rest repr)


      )

     (list

      (cond

       (= (last repr) '.) '.
       (= (last repr) '?X) (single-subs)
       (= (last repr) '(?X)) (double-subs)
       )

      )

      )

    )

  )
