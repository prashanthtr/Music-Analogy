; utility functions

;returns character at a particular position in the list
(defn charAtPos [lists pos st]

  (cond

   (empty? lists) nil
   (= pos st) (first lists)
   :else (charAtPos (rest lists) pos (+ st 1))
   )

  )


(defn roundDecimal [num]

  (if (or (float? num) (integer? num))

    0;(float (/ (round-to ( * num 100 ) 2) 100))
    0

    )
;;need to find round-to

  )

(defn anyneg [list]

  (cond

   (empty? list) false
   :else (cond

          (< (first list) 0) true
          :else (or false (anyneg (rest list)))
          )

   )

  )

;neatly display a list
(defn disp [list]

  (cond

   (true? (anyneg list)) 0
   :else list

   )

  )

;; main function that assigns a substituion cost for each of the substitutions of the forced accompaniment

                                        ; These substitution costs determine the accompaniment that the system selects or rejects. These substitution costs also determine the validity of certain discretionary choices


(defn subst-cost [sol1 sol2]

  (cond

   (= '. sol1) (cond

                (= 'ta sol2) 0.25
                (= 'tum sol2) 0.25
                (= 'te sol2) 0.5
                (= '(ta te) sol2) 0.2
                (= '(te ta) sol2) 0.2
                (= '(tum ta) sol2) 0.4
                (= '(ta tum) sol2) 0.4
                (= '. sol2) 0

                )

   ;;continuous substitutions may increase the cost of replacing a tum with ta ri
   (= 'ta sol1) (cond

                 (= 'ta sol2) 0
                 (= 'tum sol2) 0.25
                 (= 'te sol2) 0.5
                 (= '(ta te) sol2) 0.3
                 (= '(te ta) sol2) 0.3
                 (= '(tum ta) sol2) 0.4
                 (= '(ta tum) sol2) 0.4
                 :else 0

                 )
   (= 'tum sol1) (cond

                  (= 'ta sol2) 0.6
                  (= 'tum sol2) 0
                  (= 'te sol2) 0.6
                  (= '(ta te) sol2) 0.3
                  (= '(te ta) sol2) 0.3
                  (= '(tum ta) sol2) 0.4
                  (= '(ta tum) sol2) 0.4
                  :else 0

                  )
   (= 'te sol1) (cond

                  (= 'ta sol2) 0.3
                  (= 'tum sol2) 0.2
                  (= 'te sol2) 0
                  (= '(ta te) sol2)
                  (= '(te ta) sol2)
                  (= '(tum ta) sol2) 0.4
                  (= '(ta tum) sol2) 0.4
                  :else 0
                 )

   (= '(ta te) sol1) (cond

                      (= 'ta sol2) 0.2
                      (= 'tum sol2) 0.2
                      (= 'te sol2) 0.6
                      (= '(ta te) sol2) 0
                      (= '(te ta) sol2) 0.2
                      (= '(tum ta) sol2) 0.4
                      (= '(ta tum) sol2) 0.4
                      :else 0
                      )

      (= '(te ta) sol1) (cond

                      (= 'ta sol2) 0.2
                      (= 'tum sol2) 0.2
                      (= 'te sol2) 0.6
                      (= '(ta te) sol2) 0.2
                      (= '(te ta) sol2) 0
                      (= '(tum ta) sol2) 0.4
                      (= '(ta tum) sol2) 0.4
                      :else 0
                      )

      :else 1
   )

  )

;; returns the cost of substitution replacement of the substitution
(defn similarity [ sol1 sol2]

  (cond

   (empty? sol1) nil
   (= (first sol1) (first sol2)) (cons 0 (similarity (rest sol1) (rest sol2)))
   :else (cons (subst-cost (first sol1) (first sol2)) (similarity (rest sol1) (rest sol2)) )
   )

  )

;finds and displays difference between a given lead loudness and each variations of the secondary

(defn find-differ [sol var Loudness]

  (cond

   (empty? var) nil
   ;problem here
   :else (do (println (first var) " " (disp (map roundDecimal (find-differ Loudness (similarity sol (first var)))) ) )

             ;problem here
             (let [retOrnot (disp (map roundDecimal (find-differ Loudness (similarity sol (first var))
                                                     )) )]

               (cond
                (= 0 retOrnot) (find-differ sol (rest var) Loudness)
                :else (cons (first var) (find-differ sol (rest var) Loudness) )
                )
               )
             )
   )

  )

;(find-differ '(tum ta tum tum ta tum tum ta) variations '(0.9 0.3 0.5 1 0.25 0.5 1 0.5))




;;( sol = ( ta tum (?d 2) ta tum (?d 2) )
;accents are 0 and 4
; weights are ordered from 0 to 4


;returns lenght of a list

(defn lengthList [list len]

  (if (empty? list)

    len
    ( lengthList (rest list) (+ len 1))

    )

  )

(defn posChange [pos sol]

  (cond
   (empty? sol) pos
   (list? (first sol)) (posChange (map (- (first (rest (first sol)))) pos) (rest sol))
   :else (posChange pos (rest sol))
   )

  )

(defn weight-function [sol pos st]

  (println " new po" pos)

    (cond

     (>= st (lengthList sol 0)) nil
     ;accent pos
     (not= -1 (.indexOf pos st))

     (if (= st 0)

       ;first accent position starting at 0
             (let [dHit (charAtPos sol st 0)

             noDHits (cond
                      (list? dHit) (first (rest dHit))
                      :else 1
                      )

             oper (cond
                   (= noDHits 1) cons
                   :else concat
                   )
             ]

         (println "dhits" noDHits st (+ st noDHits) oper)

         (oper

          (cond
           (and (= 1 noDHits ) (not (list? dHit))) 0
           (and (= 1 noDHits ) (list? dHit))  4
           (> noDHits 1)
           (cond
                  (= noDHits 2) (list 0 0)
                  (= noDHits 3) (list 0 0 0)
                  )
           )
          (weight-function sol pos (+ st 1))
          ;(cond
           ;(= 1 noDHits) (weight-function sol pos (+ st noDHits))
           ;:else (weight-function sol pos (+ st (- noDHits 1)))
           ;)

          )

         )

       ;; other accent positions

       (let [dHit (charAtPos sol st 0)

             noDHits (cond
                      (list? dHit) (first (rest dHit))
                      :else 1
                      )

             oper (cond
                   (= noDHits 1) cons
                   :else concat
                   )
             ]

         (println "dhits" noDHits st (+ st noDHits) oper)

         (oper

          (cond
           (and (= 1 noDHits ) (not (list? dHit))) 0
           (and (= 1 noDHits ) (list? dHit))  1
           (> noDHits 1)
           (cond
                  (= noDHits 2) (list 3 3)
                  (= noDHits 3) (list 4 4 4)
                  )
           )
          (weight-function sol pos (+ st 1))
          )

         )



       )

     ;beat imm following accent pos
     (not= -1 (.indexOf (map inc pos) st))

     (let [dHit (charAtPos sol st 0)

           noDHits (cond
                    (list? dHit) (first (rest dHit))
                    :else 1
                    )

           oper (cond
                 (= noDHits 1) cons
                 :else concat
                 )
           ]

       (println "dhits" noDHits st (+ st noDHits) oper)

       (oper

        (cond
         (= 1 noDHits ) 0
         :else (cond
                (= noDHits 2) (if (not= -1 (.indexOf (map dec pos) (+ st 1) ))                                        ;true means double hit ends  1 beat before accent
                                (list 0 0)
                                (list 4 4)
                                )
                (= noDHits 3) (if (not= -1 (.indexOf (map dec pos) (+ st 2) ))                                        ;true means double hit ends  1 beat before accent
                                (list 0 0 0)
                                (list 4 4 4)
                                )
                )
         )
        (weight-function sol pos (+ st 1))
        )

       )

                                        ;beat imm before accent
     (not= -1 (.indexOf (map dec pos) st))


     (let [dHit (charAtPos sol st 0)

           noDHits (cond
                    (list? dHit) (first (rest dHit))
                    :else 1
                    )

           oper (cond
                 (= noDHits 1) cons
                 :else concat
                 )
           ]

       (println "dhits2" noDHits st (+ st noDHits) oper)

       (oper

        (cond
         (= 1 noDHits ) 0
         :else (cond
                (= noDHits 2) (if (not= -1 (.indexOf (map dec pos) (+ st 1) ))                                        ;true means 2 double hit ends  1 beat before next accent
                                (list 0 0)
                                (list 4 4)
                                )
                (= noDHits 3) (if (not= -1 (.indexOf (map dec pos) (+ st 2) ))                                        ;true means playing the 3 double hit ends  1 beat before accent
                                (list 0 0 0)
                                (list 4 4 4)
                                )
                )
         )
        (weight-function sol pos (+ st 1))
        )

       )

                                        ;beats not imm before or after accent
     :else (cons 0 (weight-function sol pos (+ st 1)) )

     )

  )
