;; Creates visualizations of the objects in the works


;;Input that needs to be updated in the diagram

                                        ;({bond-type sameness, bond-value ta, duration 1/2, obj1 {beatNo 1, diction ta, literal Q1, duration 1/4}, obj2 {beatNo 2, diction ta, literal Q1, duration 1/4}} {bond-type sameness, bond-value tum, duration 1/2, obj1 {beatNo 3, diction tum, literal Q2, duration 1/4}, obj2 {beatNo 4, diction tum, literal Q2, duration 1/4}})

(load-file "src/accompaniment_system/model3/workspace-objects.clj")

(defn tree [ip]

  (let [

        list-ip   (map

                   (fn [d]

                     (cond

                      (not= (get d 'beatNo) nil)  [ (get d 'duration) [ (get d 'literal)  [(get d 'diction) [(get d 'beatNo)] ]]
                       ]
                      :else
                      [ (get d 'duration)
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



(defn draw-l1-l2 [l1 l2]

  (let [
        treel1 (tree l1)
        treel2 (tree l2)
        ]
    (draw-tree (into [] (concat treel1 treel2)))
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
