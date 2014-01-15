;; Half notes, quarter notes notation.

;;input: sol -> ta tum tum . ta tum tum ta
;output sol ->  Q  Q   H     Q  Q   Q   Q

;;input: sol -> ta tum tum (ta te) ta tum tum ta
;output sol ->  Q  Q   Q   S S  Q   Q   Q


(defn abstrRep [sol]

  (cond

   (empty? sol) nil
   (list? (first sol)) (cons 'S (abstrRep (rest sol)))
   (= '. (first (rest sol))) (concat '(Q .) (abstrRep (rest (rest sol))) )
   :else (cons 'Q (abstrRep (rest sol)))
   )

  )

(defn hit-combos [h1 h2]

  (cond

   (= h1 'S)  (cond

              (= h2 'S) 0
              (= h2 'Q) 1
              (= h2 'H) 1
              (= h2 '.) 2
              )

   (= h2 'S)  (cond

               (= h1 'S) 0
               (= h1 'Q) 1
               (= h1 'H) 1
               (= h1 '.) 2
               )

   (= h1 '.)  (cond

              (= h2 'S) 2
              (= h2 'Q) 1
              (= h2 'H) 1
              (= h2 '.) 0
              )

   (= h2 '.)  (cond

               (= h1 'S) 2
               (= h1 'Q) 1
               (= h1 'H) 1
               (= h1 '.) 0
               )


   )

  )

(defn count-attacks [s1 s2 count]

  (cond

   (empty? s1) count
   (= (first s1) (first s2)) (count-attacks (rest s1) (rest s2) count)
   :else  (count-attacks (rest s1) (rest s2) (+ count (hit-combos (first s1) (first s2)) ) )

   )

  )


(defn count-hits [sol count]

  (cond

   (empty? sol) count
   (= (first sol) '.) (count-hits (rest sol) count)
   (= (first sol) 'S) (count-hits (rest sol) (+ count 2))
   :else (count-hits (rest sol) (+ count 1))


   )

  )

;;identical and similar Rhythm measure

(defn measure-link [sol1 sol2]

  (let [

        descr1 (abstrRep sol1)
        descr2 (abstrRep sol2)
        c1 (count-hits descr1 0)
        c2 (count-hits descr2 0)
        attacks (count-attacks descr1 descr2 0)
        ]

   (float  (* 100 (- 1 (/ attacks (+ c1 c2)))))
    )

  )


(defn attacks [rhythm attack]

  (cond
   (empty? rhythm) attack
   (list? (first rhythm)) (attacks (rest rhythm) (+ attack 2))
   (= '. (first rhythm)) (attacks (rest rhythm) attack)
   :else (attacks (rest rhythm) (+ attack 1))

   )

  )


;;returns number of mismatched attacks

(defn measure-link [r1 r2 misattack]

  (cond

   (or (empty? r1) (empty? r2)) misattack
   (= '. (first r1)) (cond

                      (= '. (first r2)) (measure-link (rest r1) (rest r2) misattack)
                      (list? (first r2)) (measure-link (rest r1) (rest r2) (+ 2 misattack) )
                      :else (measure-link (rest r1) (rest r2) (+ 1 misattack) )

                      )

   (= '. (first r2)) (cond

                      (= '. (first r1)) (measure-link (rest r1) (rest r2) misattack)
                      (list? (first r1)) (measure-link (rest r1) (rest r2) (+ 2 misattack) )
                      :else (measure-link (rest r1) (rest r2) (+ 1 misattack) )

                      )

   (list? (first r1)) (cond

                      (= '. (first r2)) (measure-link (rest r1) (rest r2) (+ 2 misattack))
                      (list? (first r2)) (measure-link (rest r1) (rest r2) misattack )
                      :else (measure-link (rest r1) (rest r2) (+ 1 misattack) )

                      )

   (list? (first r2)) (cond

                      (= '. (first r1)) (measure-link (rest r1) (rest r2) (+ 2 misattack))
                      (list? (first r1)) (measure-link (rest r1) (rest r2) misattack )
                      :else (measure-link (rest r1) (rest r2) (+ 1 misattack) )

                      )
   :else (measure-link (rest r1) (rest r2) misattack)

   )

  )

(defn score-measure-link [ r1 r2]

  (let [
        symdiff (measure-link r1 r2 0)
        a1 (attacks r1 0)
        a2 (attacks r2 0)

        ]
   (float  (* 100 (- 1 (/ symdiff ( + a1 a2 )))))
    )

  )

(defn tag-measure [r1 r2 links]

  (let [
        score (score-measure-link r1 r2)
        ]

    (cond

     (= score 100.0) {'r1-r2 'identical 'similarity 100}
     :else {'r1-r2 'similar 'similarity score}
     )

    )

  )

;returns lenght of a list

(defn lengthList [list len]

  (if (empty? list)

    len
    ( lengthList (rest list) (+ len 1))

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


;Start identical means first half is identical
;start similar means, second half is identical

(defn start-relationship [r1 r2 link]

  (let [

        l1 (lengthList r1 0)
        l2 (lengthList r2 0)
        first-r1 (subsequ r1 0 (- (/ l1 2) 1) 0)
        first-r2 (subsequ r2 0 (- (/ l2 2) 1) 0)
        last-r1 (subsequ r1 (/ l1 2) (- l1 1) 0)
        last-r2 (subsequ r2 (/ l2 2) (- l2 1) 0)
        score-first (score-measure-link first-r1 first-r2)
        score-last (score-measure-link last-r1 last-r2)
        ]

    (cond

     (= score-first 100.0) (merge link {'first 'identical})
     (= score-last 100.0) (merge link {'last 'identical})
     :else link
     )

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

(defn activity-links [rhythm dh-link ctr]

  (cond

   (> ctr (lengthList rhythm 0)) (into {} (into [] (sort (into [] dh-link))))
   (and (list? (charAtPos rhythm ctr 0)) (list? (charAtPos rhythm (+ ctr 1) 0))) (activity-links rhythm (merge {ctr (+ ctr 1)} dh-link  ) (+ ctr 1))
  :else (activity-links rhythm dh-link (+ ctr 1))

   )
  )


(defn desc-length [st len]

  (hash-map st len 'NA-start 'NA-length)

  )

; gets double hit links as input, aggregates and returns length of double hit links
(defn activity-len [dh-link st cur links ctr]

  (let [
        test-assign (get dh-link cur)
        ]

    (cond
     (= st nil) links
     (> ctr (lengthList dh-link 0)) (let [

                                          len (+ (- cur st) 1)
                                          len-struct (desc-length st len)
                                          ]
                                      (merge links len-struct)
                                      )
     (not= nil test-assign) (activity-len dh-link st test-assign links (+ ctr 1))
     :else (let [

                 len (+ (- cur st) 1)
                 len-struct (desc-length st len)
                 new-st (first (into [] (charAtPos dh-link (- ctr 1) 0)))

                 ]
             (activity-len dh-link new-st new-st (merge links len-struct) ctr)
             )
     )

    )

  )

(defn note-activity [r1]

  (let [

        act-link (activity-links r1 {} 0)
        st (cond
            (empty? act-link) nil
            :else (first (into [] (charAtPos act-link 0 0)))
            )
        activity-descr (activity-len act-link st st {} 1)
        ]
    activity-descr
    )

  )

;; map -> vector of vectors -> set of vectors -> intersction -> set of vectors -> map
(defn mapTosetVector [r1-map]

  (into #{} (into [] r1-map) )
  )

(defn setVectorTomap [set-vect]

  (into {} set-vect)

  )

(defn common-note-activity [ r1 r2 links]

  (let [
        act-desc-r1 (mapTosetVector (note-activity r1))
        act-desc-r2 (mapTosetVector (note-activity r2))
        intersection (clojure.set/intersection act-desc-r1 act-desc-r2)
        intersection-map (setVectorTomap intersection)
        intersection-map (cond

                          (empty? intersection-map) {}
                          :else (merge {'Identical 'NA} intersection-map)
                          )
        difference (cons (clojure.set/difference act-desc-r1 act-desc-r2) (clojure.set/difference act-desc-r2 act-desc-r1))
        difference-map (setVectorTomap difference)
        difference-map (cond

                        (empty? difference-map) {}
                        :else (merge {'diff 'NA} difference-map)
                        )
        ]
    (println difference)
    (merge intersection-map links difference-map)

    )

  )

(defn integrate [r1 r2]

  (let [

        measure-links (tag-measure r1 r2 0)
        measure-links (start-relationship r1 r2 measure-links)
        measure-links (common-note-activity r1 r2 measure-links)
        ]
    measure-links
    )

  )
