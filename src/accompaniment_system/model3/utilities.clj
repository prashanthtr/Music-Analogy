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

(defn printneat [list]

  (cond

   (empty? list) nil
   (= nil (first list)) (printneat (rest list))
   :else (do (println (first list) ) (printneat (rest list)) )
   )

  )


(defn clean-rep [lists]

  (cond

   (empty? lists) nil
   (= nil (first lists))  (clean-rep (rest lists))
   :else (cons (first lists) (clean-rep (rest lists)))
   )

  )
