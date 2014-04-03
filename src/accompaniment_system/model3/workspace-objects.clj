;; Code creates workspace objects given the diction as input

(load-file "src/accompaniment_system/model3/literal-abstraction.clj")
(load-file "src/accompaniment_system/model3/object-desc.clj")
(load-file "src/accompaniment_system/model3/utilities.clj")


; Code that gets input, creates a record of type atomic-objects

(defn beatNum [len]

  (let [
        integers (iterate inc 1)
        ]
    (take len integers)
    )
  )

(defn d2Duration [d]

  (cond

   (list? d) 1/4
   :else 1/4
   )

  )

(defn create-atomic-objects [diction]

  (let [
        lit-abstr (literal-abstr diction)
        ]

    (map

     (fn [d bn l]
       (cond

        (list? d) {'beatNo bn 'diction d 'literal l 'duration 1/4 'noteDuration 'EE}
        :else {'beatNo bn 'diction d 'literal l 'duration 1/4 'noteDuration 'Q} ; partially production rules used here
        )

       )
     diction (beatNum (lengthList diction 0)) lit-abstr
     )
    )

  )


;; Formed by identifying sameness or pitch bend relations between atomic objects

(defn create-composite-objects [atomic-obj]

  (map

   (fn [a1 a2]

     (let [
           d1 (get a1 'diction)
           d2 (get a2 'diction)
           ]
       (cond

        (= d1 d2) {'bond-type 'sameness 'bond-value d1 'duration 1/2 'noteDuration 'H 'obj1 a1 'obj2 a2}
        (and (not= d1 d2) (= d2 '.)) {'bond-type 'pitch-bend 'bond-value d1 'duration 1/2 'noteDuration 'H 'obj1 a1 'obj2 a2}
        :else nil
        )
       )
     )
   atomic-obj (rest atomic-obj)
   )


  )


;; Composite objects made of other composite objects

(defn l2-composite-objects [diction]


  (let [
        atomic (create-atomic-objects diction)
        composite (create-composite-objects atomic)
        ccomposite     (map

                        (fn [a1 a2]

                          (cond

                           (or (= a1 nil) (= a2 nil)) nil
                           :else

                           (let [
                                 d1 (get a1 'bond-value)
                                 d2 (get a2 'bond-value)
                                 ]
                             (cond

                              (= d1 d2) {'bond-type 'sameness 'bond-value d1 'duration 3/4 'noteDuration 'HQ 'obj1 (get a1 'obj1) 'obj2 (get a1 'obj2) 'obj3 (get a2 'obj2)  }
                              :else nil
                              )
                             )
                           )

                          )
                        composite (rest composite)
                        )

        ]
    (concat atomic composite ccomposite)
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
