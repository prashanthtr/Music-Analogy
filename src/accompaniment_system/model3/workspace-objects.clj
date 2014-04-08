;; Code creates workspace objects given the diction as input

(load-file "src/accompaniment_system/model3/utilities.clj")
(load-file "src/accompaniment_system/model3/literal-abstraction.clj")
(load-file "src/accompaniment_system/model3/object-desc.clj")


; Code that gets input, creates a record of type atomic-objects



(defn beatNum [len x]

  (let [
        integers (iterate (partial + x) 1)
        ]
    (take len integers)
    )
  )

(defn symDur [d]

  (cond

   (= d 1/8) 'EE
   (= d 1/4) 'Q
   (= d 1/2) 'H
   (= d 3/4) (rand-nth '(HQ QH))
   (= d 4/4) (rand-nth '(HH HQQ QHQ QQH))
   )

  )

;; Atomic objects have duration of 1/4 or 1/8. If its 1/4 its a Q. 1/8 then its an EE.
;; Composite objects have duration of H, HQ or HHQ etc. There are composed of atomic objects linked together by sameness or pitch bend relations.



;; Diction -> list of maps
;;
(comment

   1 {diction ta, literal Q1, duration 1/4, noteDuration Q}
   2 {diction tum, literal Q1, duration 1/4, noteDuration Q}
   3 {diction tum, literal Q1, duration 1/4, noteDuration Q}

   (2 3) {bond-type sameness, bond-value tum, duration 1/2, noteDuration H}
   4
)

(defn create-atomic-objects [diction]

  (let [
        lit-abstr (literal-abstr diction)
        atomic-obj (map

                    (fn [d bn l]
                      (cond

                       (list? d) {bn {'diction d 'literal l 'duration 1/4 'noteDuration 'EE}}
                       :else {bn {'diction d 'literal l 'duration 1/4 'noteDuration 'Q}} ; partially production rules used here
                       )

                      )
                    diction (beatNum (lengthList diction 0) 1) lit-abstr
                    )
        ]
    (into {} atomic-obj)
    )

  )

;; Formed by identifying sameness or pitch bend relations between atomic objects


(defn create-l1composite
  [atomic-obj bn]

  (reduce merge

          (map

           (fn [bn1 bn2]

             (let [
                   d1 (get (get atomic-obj bn1) 'diction)
                   d2 (get (get atomic-obj bn2) 'diction)
                   l1 (get (get atomic-obj bn1) 'literal)
                   l2 (get (get atomic-obj bn2) 'literal)
                   ]
               (cond

                (= d1 d2) { (list bn1 bn2) {'bond-type 'sameness 'bond-value d1 'duration 1/2 'noteDuration 'H 'literal (list l1 l2) 'diction (list d1 d2) } }
                (and (not= d1 d2) (= d2 '.)) { (list bn1 bn2) {'bond-type 'pitch-bend 'bond-value d1 'duration 1/2 'noteDuration 'H 'literal (list l1 l2) 'diction (list d1 d2)}}
                :else nil
                )
               )
             )
           bn (rest bn)
           )

          )
  )

;(nil {(2 3) {bond-type pitch-bend, bond-value tum, duration 1/2, noteDuration H}} nil)

(defn create-l2composite
  [atomic-obj bn]

  (reduce merge

          (map

           (fn [bn1 bn2]

             (let [
                   d1 (cond
                       (not= nil (get atomic-obj (list bn1 bn2)))
                       (get (get atomic-obj (list bn1 bn2)) 'bond-type)
                       :else nil
                       )
                   d2 (cond
                       (not= nil (get atomic-obj (list  bn2 (+ bn2 1))))
                       (get (get atomic-obj (list bn2 (+ bn2 1))) 'bond-type)
                       )

                   b1 (cond

                       (not= nil d1) (get (get atomic-obj (list bn1 bn2)) 'bond-value)
                       :else nil
                       )

                   b2  (cond

                        (not= nil d2) (get (get atomic-obj (list  bn2 (+ bn2 1))) 'bond-value)
                        :else nil
                        )
                   l1 (cond

                       (not= nil d1) (get (get atomic-obj (list  bn1 bn2)) 'literal) ;list
                       :else nil
                       )
                   l2 (cond

                       (not= nil d2) (get (get atomic-obj (list  bn2 (+ bn2 1))) 'literal) ;list
                       :else nil
                       )

                   dn1 (cond

                             (not= nil d1) (get (get atomic-obj (list  bn1 bn2)) 'diction) ;list
                             :else nil
                             )
                   dn2 (cond

                             (not= nil d2) (get (get atomic-obj (list  bn2 (+ bn2 1))) 'diction) ;list
                             :else nil
                             )

                   comb-dc (cond

                             (and (list? dn1) (list? dn2)) (concat dn1 (rest dn2))
                             (and (not (list? dn1)) (list? dn2)) (cons dn1 dn2)
                             :else (concat dn1 (list dn2))
                             )

                   comb-lit (cond

                             (and (list? l1) (list? l2)) (concat l1 (rest l2))
                             (and (not (list? l1)) (list? l2)) (cons l1 l2)
                             :else (concat l1 (list l2))
                             )
                   duration (* 1/4 (- (+ bn2 2) bn1) )

                   ]
               (cond

                (and (not= nil d1) (not= nil d2))
                (cond

                 (and (= d1 d2) (= b1 b2))
                 { (list bn1 (+ bn2 1)) {'bond-type 'sameness 'bond-value d1 'duration duration 'noteDuration (symDur duration) 'literal comb-lit 'diction comb-dc} }
                 (and (= d1 'sameness) (= d2 'pitch-bend) )
                 { (list bn1 (+ bn2 1)) {'bond-type 'pitch-bend 'bond-value d1 'duration duration 'noteDuration (symDur duration) 'literal comb-lit 'diction comb-dc}  }

                 :else nil
                 )
                :else nil
                )

               )
             )
           bn (rest bn)
           )


          )

  )

;; Composite objects made of other composite objects
;; Code at the moment looks at only 3 levels
(defn l2-composite-objects [diction]

  (let [
        atomic (create-atomic-objects diction)
        composite-l1 (create-l1composite atomic (beatNum (lengthList diction 0) 1))
        composite-l2 (create-l2composite composite-l1 (beatNum (lengthList diction 0) 1))
        ]

    (merge atomic composite-l1 composite-l2)
    )
  )


(defn get-note-dur [list-maps dur]

  (cond
   (empty? list-maps) dur
   (= nil (first list-maps)) nil
   :else
   (let [
         note-dur (get (first list-maps) 'duration)
         ]
     (get-note-dur (rest list-maps) (+ dur note-dur))
     )
   )
  )

(defn sort-list-maps [list-maps]

  (let [

        nilcheck  (cond

                   (= (.indexOf list-maps nil) -1) true
                   :else false
                   )

        results  (cond

                  (= nilcheck true)
                  (map

                   (fn [d]

                     (cond

                      (not= nil (get d 'beatNo)) {(get d 'beatNo) d}
                      :else {(get (get d 'obj1) 'beatNo) d}
                      )

                     )
                   list-maps)

                  :else nil
                  )

        ]
    ;(printneat results)
    (cond

     (= results nil) nil
     :else
     (map last

          (into (sorted-map-by (fn [key1 key2]
                                 (compare
                                  [(get results key1) key1]
                                  [(get results key2) key2])
                                 )
                               )
                results)

          )

          )
    )

  )

(defn check-order [list-maps]

  (cond
   (empty? (rest list-maps)) true
   (= nil (first list-maps)) nil
   :else
   (let [

         flm (first list-maps)
         flm2 (first (rest list-maps))
         b1 (get (first list-maps) 'beatNo)
         b2 (get (first (rest list-maps)) 'beatNo)

         b2 (cond

             (= b2 nil) (cond
                         (or (= (get flm2 'bond-type) 'sameness)
                             (= (get flm2 'bond-type) 'pitch-bend)
                             )
                         (get (get flm2 'obj1) 'beatNo)
                         :else nil
                         )
             :else b2
             )
         b1 (cond

             (= b1 nil) (cond
                         (and
                          (or (= (get flm 'bond-type) 'sameness)
                              (= (get flm 'bond-type) 'pitch-bend) )
                          (not= (get (get flm 'obj2) 'beatNo) b2)
                          )
                         (get (get flm 'obj1) 'beatNo)
                         :else nil
                         )
             :else b1
             )
         ]
    ; (println "here" b1 b2)

     (cond

      (or (= b1 nil) (= b2 nil)) nil
      (= b1 b2) nil
      (> b1 b2) nil
      :else (check-order (rest list-maps))
      )
     )
   )
  )


(defn check-consistency [d1]

  (let [
        sort-d1 (sort-list-maps d1)
        ]
    (cond

     (and (= (get-note-dur sort-d1 0) 1)
          (= (check-order sort-d1) true))
     sort-d1
     :else nil
     )
    )
  )

(defn all-representations [diction]

  (let [
        repr (l2-composite-objects diction)
        combinations2 (combinations repr 2)
        valid-combinations2     (map

                                 (fn [d1]
                                   (check-consistency d1)
                                   )
                                 combinations2
                                 )
        combinations3 (combinations repr 3)
        valid-combinations3
        (map

         (fn [d1]
           (check-consistency d1)
           )
         combinations3
         )

        combinations4 (combinations repr 4)
        valid-combinations4
        (map

         (fn [d1]
           (check-consistency d1)
           )
         combinations4
         )
        ]
    ;(println " output")
    ;(printneat (distinct (concat valid-combinations2 valid-combinations3 valid-combinations4)))
    (distinct (concat valid-combinations2 valid-combinations3 valid-combinations4))
    )

  )

;Takes in 2 objects of a string, descriptor-types, gets relevant descriptions and returns a bond based on the descriptor types.
; bond is either sameness, difference


(defn inner-bonds [obj1 obj2 d-type]

  (let [
        d1 (get obj1 d-type)
        d2 (get obj2 d-type)
        relation (cond

                  (= d1 d2) 'sameness
                  :else 'difference
                  )
        ]
    (str relation " of " d-type " between " (get obj1 'beatNo) " and " (get obj2 'beatNo))
    )
  )



(defn create-arr [val]
  (cons val (lazy-seq (create-arr val)))
  )


;;need to fix it for literal
(defn ret-relevant-descriptions [d-type obj]

  (map

   (fn [d1]


     (let [
           val (get d1 d-type)
           dur (get d1 'duration)
           ]
       (cond
        (not= val nil) val
        :else (cond

               (and (= d-type 'literal)  (= nil (get d1 d-type)))
               ; object is composite
               (list (get (get d1 'obj1) d-type) (get (get d1 'obj2) d-type))
               (not= d-type 'diction) (get d1 d-type)
               :else (cond
                                        ; composite object
                      (= (get d1 'bond-type) 'sameness)
                      ( take (* dur 4) (create-arr (get d1 'bond-value))  )
                      :else
                      (cons (get d1 'bond-value) (take (- (* dur 4) 1) (create-arr '.) ))
                      )

               )
        )
       )
     )

   obj)
  )


(defn check-duration [list]

  (let [
        sum (reduce + (map

                       (fn [d]

                         (cond

                          (list? d) (- (+ 1 (last d)) (first d) )
                          :else 1
                          )
                         )
                       list)
                    )
        ]
    (cond

     (= sum 4) true
     :else nil
     )
    )

  )

(defn consit [list]

  (cond

   (empty? list) nil
   (list? (first list)) (concat (first list) (consit (rest list)))
   :else (cons (first list) (consit (rest list)))
   )
  )


(defn c1-check [d]

    (let [
          d1 (check-duration d)
          ]
      (cond
       (= d1 true) d
       :else nil
       )
      )

    )

(defn ret-dur [list]

    (let [
        sum (reduce + (map

                       (fn [d]

                         (cond

                          (list? d) (- (+ 1 (last d)) (first d) )
                          :else 1
                          )
                         )
                       list)
                    )
          ]
      sum
      )

  )



(defn available-bn-descriptions [diction]

  (let [
        av-bn (map first (reduce conj [] (l2-composite-objects diction) ))
        c3 (combinations av-bn 3)
        ic3 (map
             (fn [d]
               (c1-check d)
               )
             c3)

        valid-c3 (map

                  (fn [d]

                    (let [
                          d1 (distinct (consit d))
                          len (lengthList d1 0)
                          d1 (cond

                              (= len (ret-dur d)) true
                              :else nil

                              )
                          ]
                      (cond
                       (true? d1) d
                       :else nil
                       )
                      )
                    )
                  ic3
                  )

        c2 (combinations av-bn 2)
        ic2 (map
             (fn [d]
               (c1-check d)
               )
             c2)

        valid-c2 (map

                  (fn [d]

                    (let [
                          d1 (distinct (consit d))
                          len (lengthList d1 0)
                          d1 (cond

                              (= len (ret-dur d)) true
                              :else nil

                              )
                          ]
                      (cond
                       (true? d1) d
                       :else nil
                       )
                      )
                    )
                  ic2
                  )
        c4 (combinations av-bn 4)
        ic4 (map
             (fn [d]
               (c1-check d)
               )
             c4)

        valid-c4 (map

                  (fn [d]

                    (let [
                          d1 (distinct (consit d))
                          len (lengthList d1 0)
                          d1 (cond

                              (= len (ret-dur d)) true
                              :else nil

                              )
                          ]
                      (cond
                       (true? d1) d
                       :else nil
                       )
                      )
                    )
                  ic4
                  )

        ]
    (clean-rep (concat valid-c2 valid-c3 valid-c4) )
    )
  )


;; Still need to get from individual objects to the rhythm bar
;; sorting a list containing input like (1 2 (3 4) )
; Algorithm - create index list with keys as 1,2,3 etc and values as 1,2,3 and sort
(defn sort-list [list]

  (let [
        map-ind (into {} (map-indexed vector list))
        ;; map that can be sorted
        map2 (map
              (fn [i]
                (let [
                      ind (- i 1)
                      el (get map-ind ind)
                      ]
                  (cond
                   (list? el) {ind (first el)}
                   :else {ind el}
                   )
                  )
               )
              (beatNum (lengthList list 0) 1))
        m2 (reduce merge map2)
        sorted-m2 (sort-by val < m2)
        keys (map first sorted-m2)
        ; m2 to sorted list based on key

        sorted-list (map

                     (fn [d]
                       (get map-ind d)
                       )
                     keys)

        ]
    sorted-list
    ;(println "map2" map2 "m2" m2 "sorted-m2" sorted-m2 "map ind" map-ind)
    ;use sorted map as key to generate list
    )
  )


(defn descriptions [diction]

  (let [
        l2-comp (l2-composite-objects diction)
        desc (available-bn-descriptions diction)
        desc (map
                     (fn [d]
                       (sort-list d)
                       )
                     desc)

        description (map

         (fn [d]

           (map

            (fn [d1]

              (merge (get l2-comp d1) {'beatNo d1})

              )
            d)

           )

         desc)
        ]

    ;(printneat description)
    ;(println desc)
    description
    )
  )

;; This is a part of a separate selection module. Object input is a full description of all the four beats
(defn ret-descriptions [obj desc-type]

  (map

   (fn [d]

     (get d desc-type)

     )

   obj)
  )
