(load-file "src/accompaniment_system/generate.clj")


;; input : sol

;; output: possible groupings and what they are grouped based on

;;groups the groove based on double hits

(defn doublehit-group [sol st]

  (cond

   (>= st (lengthList sol 0)) nil
   (and (list? (charAtPos sol st 0)) (list? (charAtPos sol (+ st 1) 0)) (list? (charAtPos sol (+ st 2) 0))) (cons (list (charAtPos sol st 0) (charAtPos sol (+ st 1) 0) (charAtPos sol (+ st 2) 0) ) (doublehit-group sol (+ st 3)) )
   (and (list? (charAtPos sol st 0)) (list? (charAtPos sol (+ st 1) 0)) ) (cons (list (charAtPos sol st 0) (charAtPos sol (+ st 1) 0) ) (doublehit-group sol (+ st 2)) )
   (list? (charAtPos sol st 0)) (cons (list (charAtPos sol st 0)) (doublehit-group sol (+ st 1))  )
   :else (cons (charAtPos sol st 0) (doublehit-group sol (+ st 1))  )
   )

  )

;;groups the groove based on accents
(defn accent-group [sol accent]

  (cond

   (empty? accent) nil
   (empty? (rest accent)) (cons (subsequ sol (first accent) (- (lengthList sol 0) 1) 0 )  (accent-group sol (rest accent)) )
   (= 0 (first accent)) ( cons (subsequ sol (first accent) (- (first (rest accent)) 1) 0 ) (accent-group sol (rest accent) ) )
   :else ( concat (subsequ sol (first accent) (- (first (rest accent)) 1) 0 ) (accent-group sol (rest accent) )   )

   )

  )

(defn doubleinaccent [sol]

  (cond

   (empty? sol) nil
   :else (cons (doublehit-group (first sol) 0) (doubleinaccent (rest sol)))

   )

  )


;; does the order bonds
(defn interpretations [sol accent]

  (cond

   (empty? accent) nil
   :else

   (let [accentgrp (cond
                    (= (first (first accent)) 0) (accent-group sol (first accent))
                    :else (accent-group sol (cons 0 (first accent)))
                    )
         dblgroup (doubleinaccent accentgrp)
         ]
     ;(println accentgrp)
     (cons dblgroup (interpretations sol (rest accent)) )
     )

   )

  )

(defn check-len [sol]

  (println sol)
  (> (lengthList sol 0) 1) (check-len (first sol))
  :else sol
  )

;; bind a variable to a pattern and find if the same binding holds for the rest of the pattern, else extend the binding to include next element and check the rest of the pattern

(defn check-binding [list binding]

  ;(println list binding)
  (let [bind (get binding list)]

    (cond

     (= nil bind) false
     :else true
     )
    )
  )

(defn symm [sol binding st]

  (let [list (check-len sol)
        seq (subsequ sol 0 st 0)
        binding {seq '?sym}
        ]

    (println list)
    ;(println binding)
    (cond

     (> st (lengthList list 0)) nil
     (check-binding (subsequ list (+ st 1) (- (lengthList list 0) 1) 0) binding) binding
     :else (symm list binding (+ st 1))

     )

    )


  )

; checks and creates the symmetry bindings if any for each of the list
(defn symm-bonds [lists]

  (cond

   (empty? lists) nil
   :else  (cons (symm (first lists)) (symm-bonds (rest lists)) )
   )

  )

;;creates inorder groupings using double hits and accent groupings
(defn inorder-bonds [sol accents subst]


  (let [groupings (interpretations sol accents)]

    (symm-bonds (cons sol groupings))

    )

  )
