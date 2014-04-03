; Contains a heirarchy of objects needed to automaticaly select the rules for transformations.

(def description-type ::description-type)

(def diction ::diction)
;secondary
(derive ::ta diction )
(derive ::te diction )
(derive ::tum diction )
(derive ::. diction )

;lead
(derive ::na diction )
(derive ::dhim diction )
(derive ::dheem diction )
(derive ::thi diction )
(derive ::tha diction )
(derive ::ri diction )
(derive ::thom diction )
(derive ::ka diction )
(derive ::dhin diction )
(derive ::dham diction )


(def noteDuration ::NoteDuration)
(derive ::Q noteDuration )
(derive ::H noteDuration )
(derive ::EE noteDuration )
(derive ::HQ noteDuration )

(def inner-bonds ::inner-bonds)
(derive ::sameness inner-bonds )
(derive ::difference inner-bonds )


(derive diction description-type)
(derive noteDuration description-type)
(derive inner-bonds description-type)


;({beatNo 1, diction ta, literal Q1, duration 1/4, Abstr Q} {bond-type sameness, bond-value tum, duration 1/2, Abstr H, obj1 {beatNo 2, diction tum, literal Q2, duration 1/4, Abstr Q}, obj2 {beatNo 3, diction tum, literal Q2, duration 1/4, Abstr Q}} {beatNo 4, diction ta, literal Q1, duration 1/4, Abstr Q})



(defn create-arr [val]
  (cons val (lazy-seq (create-arr val)))
  )

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


(defmulti T-rule description-type)

(defn NR-rule [d1 d2]

  {description-type diction :beatNo d1 :replace d2}

  )

(defn R-rule [d1 d2]

  {description-type noteDuration }
  )

(defmethod T-rule diction [obj]

  (str "replace beat Number " (:beatNo obj) " by " (:replace obj))

  )

(defmethod T-rule noteDuration [d1 d2]

  (str "replace beat Number " d1 " by " d2)

  )
