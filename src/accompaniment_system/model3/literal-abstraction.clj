;; Rules for literal abstraction from concrete diction

;; Input:  ta tum tum tate ta tum tum ta
;; Output: Q1 Q2  Q2  E1E2 Q1 Q2  Q2  Q1

;; Method: Using a context sensitive substituion rule by maintaining a list of asociations


(defn check-assoc [hit assoc]

  (if
      (= nil (get assoc hit)) true
      nil
      )

  )


  (defn build-dict-sym-assoc [diction assoc n1 n2]

    (let [
          hit (first diction)
          ]
      (cond

       (= nil hit) assoc
       (= nil (get assoc hit))

       (cond

        ;eighth notes
        (list? hit )
        (let [
              h1 (first hit)
              h2 (last hit)

              literal (cond

                       (= h1 h2) (clojure.string/join (list 'E (+ n2 1) 'E (+ n2 1)))
                       :else (clojure.string/join (list 'E (+ n2 1) 'E (+ n2 2)))
                       )
                                        ;creating the index for the new literal
              n2 (cond

                  (= h1 h2) (+ n2 1)
                  :else (+ n2 2)

                  )
              ]
          (build-dict-sym-assoc (rest diction) (merge assoc {hit literal}) n1 n2 )
          )
        :else
        ;quarter notes
        (let [
              literal (clojure.string/join (list 'Q (+ n1 1))) ; creating the index for the new literal
              ]
          (build-dict-sym-assoc (rest diction) (merge assoc {hit literal}) (+ n1 1) n2)
          )


        )
       :else (build-dict-sym-assoc (rest diction) assoc n1 n2)
       )
      )

    )

(defn literal-abstr [diction]

  (let [

        assoc (build-dict-sym-assoc diction {} 0 0)
        strs     (map
                  (fn [d]

                    (get assoc d)

                    )
                  diction)
        la-symbols (map symbol strs)
        ]
    la-symbols
    )
  )
