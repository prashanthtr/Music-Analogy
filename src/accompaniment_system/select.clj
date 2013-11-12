; utility functions

(defn roundDecimal [num]

  (if (or (float? num) (integer? num))

    (float (/ (round-to ( * num 100 ) 2 ) 100))
    0

    )


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

                                        ; finds and displays difference between a given lead loudness and each variations of the secondary

(defn find-differ [sol var Loudness]

  (cond

   (empty? var) nil
   :else (do (println (first var) " " (disp (map roundDecimal (differ Loudness (similarity sol (first var)))) ) )

             (let [retOrnot (disp (map roundDecimal (differ Loudness (similarity sol (first var)))) )]

               (cond
                (= 0 retOrnot) (find-differ sol (rest var) Loudness)
                :else (cons (first var) (find-differ sol (rest var) Loudness) )
                )
               )
             )
   )

  )

;(find-differ '(tum ta tum tum ta tum tum ta) variations '(0.9 0.3 0.5 1 0.25 0.5 1 0.5))
