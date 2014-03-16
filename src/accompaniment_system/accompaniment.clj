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





;accompaniment with note duration only

(defn dict2Rep [diction]

  (let [
        f (first diction)
        s (first (rest diction))
        t (first (rest (rest diction)))
        fr (first (rest (rest (rest diction))))
        ]

    (cond

     (empty? diction) nil
     (and  (not= f '.) (= s '.) (not= s nil) ) (cond

                                   (not= t '.) (cons 'H (dict2Rep (rest (rest diction)))) ; 2 beats
                                   (= t '.) (cond

                                             (not= fr '.) (cons 'HQ (dict2Rep (rest (rest (rest diction)))))
                                             (= fr '.) (cons 'HH (dict2Rep (rest (rest (rest (rest diction))))))
                                             :else (cons 'HQ (dict2Rep (rest (rest (rest diction)))))
                                             )
                                   :else (cons 'H (dict2Rep (rest (rest diction)))) ; 2 beats
                                   )
     :else (cond

            (list? f) (cond

                       (= (lengthList f 0) 2) (cons '(S S) (dict2Rep (rest diction)) )  ;sixteenth
                       (= (lengthList f 0) 3) (cons '(Tr Tr Tr) (dict2Rep (rest diction)) )  ;
                       (= (lengthList f 0) 4) (cons '(Qr Qr Qr Qr) (dict2Rep (rest diction)))
                       )
            :else (cons 'Q (dict2Rep (rest diction)) )  ; single beat
            )
     )

    )

  )



(defn gen-alt-rep [sym]

  (cond

   (= sym 'H) '(Q Q)
   (= sym 'HQ) (rand-nth '((Q Q Q) (H Q)))
   (= sym 'QH) (rand-nth '((Q Q Q) (Q H)))
   (= sym 'HH) (rand-nth '((Q Q Q Q) (Q Q H) (H Q Q) (Q H Q) (QH Q) (HQ Q) (Q QH) (Q HQ)))
   )

  )

(defn single-list [diction]

  (cond
   (empty? diction) nil
   (list? (first diction)) (concat (first diction) (single-list (rest diction)))
   :else (cons (first diction) (single-list (rest diction)))
   )

  )

;(change-rep '(ta . tum tum ta tum . ta))
(defn change-rep [diction]

  (let [

        rep (dict2Rep diction)
        alt-rep  (map (fn [d]
                        (cond

                         (= d 'Q) d
                         :else (gen-alt-rep d)
                         )
                        )
                      rep
                      )

        ]
    (println "original" rep)
    (println "alteredl" alt-rep)
    (single-list alt-rep)
    )

  )

(defn match [d1 d2]

  (if

      (= (lengthList d1 0) (lengthList d2 0)) true
      false
      )

  )

(defn find-association [diction1 diction2 st1 st2 ctr]

  (let [
        d1 (charAtPos diction1 st1 0)
        d2 (charAtPos diction2 st2 0)

        ]
    ;(println d1 d2 st1 st2 ctr)
    (cond

     (or (>= st1 (lengthList diction1 0)) (>= st2 (lengthList diction2 0))) nil
     (= d1 d2) (cons {d1 d2} (find-association diction1 diction2 (+ st1 1) (+ st2 1) ctr ) )
     :else (cond

            (< (/ d2 d1) 1) (let [

                                  sseq (subsequ diction2 st2 (+ st2 ctr) 0)
                                  sum (reduce + sseq)
                                  ]
                              (cond

                               (= (/ sum d1) 1) (cons {d1 sseq} (find-association diction1 diction2 (+ st1 1) (+ st2 (+ ctr 1)) 1) )
                               :else (find-association diction1 diction2 st1 st2 (+ ctr 1))
                               )

                              )

            (> (/ d2 d1) 1) (let [

                                  sseq (subsequ diction1 st1 (+ st1 ctr) 0)
                                  sum (reduce + sseq)
                                  ]
                              (cond

                               (= (/ d2 sum) 1) (cons {sseq d2} (find-association diction1 diction2 (+ st1 (+ ctr 1)) (+ st2 1) 1) )
                               :else (find-association diction1 diction2 st1 st2 (+ ctr 1))
                               )

                              )


            )


    )

  ))



(defn diction-bonds [diction]

  (let [

        d1 (first diction)
        d2 (first (rest diction))

        ]
    (cond

     (empty? (rest diction)) nil
     (and (= d1 d2) (not= d1 'd) (not= d2 'd)) (cons 's (gen-sim-dif (rest diction)))
     (= d2 '.) (cons 's (gen-sim-dif (rest diction)))
     :else (cons 'd (gen-sim-dif (rest diction)))
     )

    )

  )

(defn recur-dc [dc level]

  (let [
        cur-dc dc
        next-dc (diction-bonds cur-dc)
        ]
    (cond

     (not= (.indexOf next-dc 's) -1) (cons {'level level 'bonds cur-dc} (recur-dc next-dc (+ level 1)) )
     :else (list {'level level 'bonds cur-dc})

     )

    )
  )

(defn slippability [diction]


  (let [

        repr (dict2Rep diction)
        bonds (recur-dc (diction-bonds diction))
        ]


    )
  )
