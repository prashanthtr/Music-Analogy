;accompaniment with note duration only


(defn l1-abstraction [diction]

  diction

  )


(defn l2-abstraction [literal]

  (map )

  )



;; Contains a list of production rules, that guide the L2 abstraction

(defn production-rules [Ip]

  (cond

   ;; There are 4 distinct Q's for secondary and 7 distinct Q's for lead
   (= Ip 'Q1) 'Q  ;; each for a distinct hit - ta, te, tum, . , tumki, tha,thi, nam, dhim, tham etc
   (= Ip 'Q2) 'Q
   (= Ip 'Q3) 'Q
   (= Ip 'Q4) 'Q
   (= Ip 'Q5) 'Q
   ;; lead
   (= Ip 'Q6) 'Q
   (= Ip 'Q7) 'Q
   (= Ip 'Q8) 'Q
   (= Ip 'Q9) 'Q
   (= Ip 'Q10) 'Q
   (= Ip 'Q11) 'Q
   (= Ip 'Q12) 'Q

   ;; There are 2 H's for secondary and 5 for lead.

   (= Ip 'H1) 'H  ;; each for a distinct hit that can be half note- dhim, tham, tum, dhom, nam, dheem, dham
   (= Ip 'H2) 'H
   (= Ip 'H3) 'H
   (= Ip 'H4) 'H
   (= Ip 'H5) 'H
   (= Ip 'H6) 'H
   (= Ip 'H7) 'H


   (= Ip 'E1E2) 'EE
   (= Ip 'E2E1) 'EE
   (= Ip 'E1E3) 'EE
   (= Ip 'E3E1) 'EE
   (= Ip 'E2E3) 'EE
   (= Ip 'E3E2) 'EE

   )
  )

(defn representation [concrete]

  (let [

        literal-notation (l1-abstraction concrete)
        abstr-notation (l2-abstraction literal-notation)

        ])

  )

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


Regular grammar going to be useful in finding out patterns of occurences in the literal notation.


Q1 Q2 Q2 Q3 -> Q H Q - using production rule Q2 Q2 -> H
Q1 Q2 Q2 Q3 -> Q Q Q Q - using prod rule Q1 -> Q
Q1 Q2 Q2 Q3 -> Q Q H - Using prod rule Q2 Q3 -> H

What I need to have is rules that can be used to build the grammar

3 main rules

Q1 -> Q
Q\d -> Q

cartesian product of number of 4 combinations of Q,H,EE with number of 4 combinations of Q,H and EE
