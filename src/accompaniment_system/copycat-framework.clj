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

(defn check-binding [var list binding]

  ;(println list binding)
  (let [bind (get binding var)]

    (cond

     (not= list bind) false
     :else true
     )
    )
  )

(defn check-binding-compl [var list binding]

  ;(println list binding)
  (let [bind (get binding var )]

    (cond
     (not= (reverse list) bind) false
     :else true
     )
    )
  )

(defn symm [list bindings st var chk-binding]

  (let [;list (check-len sol)
        seq (subsequ list 0 st 0)
        binding {var seq}
        ]

    ;(println list)
    ;(println binding)
    (cond

     (>= st (lengthList list 0)) bindings
     (chk-binding var (subsequ list (+ st 1) (- (lengthList list 0) 1) 0) binding) (do (println var binding) (merge binding bindings) )
     :else (symm list bindings (+ st 1) var chk-binding)

     )

    )


  )

;; takes a pattern and finds the symmetry across whole, and half patterns
(defn sym-sol [sol bindings]


  (let [
        firsthalf (subsequ sol 0 3 0)
        secondhalf (subsequ sol 4 (- (lengthList sol 0) 1) 0)
        cb check-binding
        ccb check-binding-compl
        b1 (symm sol bindings 0 '(same whole) cb )
        b2 (symm firsthalf b1 0 '(same first) cb)
        b3 (symm secondhalf b2 0 '(same second) cb)
        b1 (symm sol bindings 0 '(opp whole) ccb )
        b2 (symm firsthalf b1 0 '(opp first) ccb)
        b4 (symm secondhalf b2 0 '(opp second) ccb)

        ]
    (merge b3 b4)
    )

  )

; checks and creates the symmetry bindings if any for each of the list
(defn symm-bonds [lists]

  (cond

   (empty? lists) nil
   :else  (cons (sym-sol (first lists) {}) (symm-bonds (rest lists)) )
   )

  )

;;creates inorder groupings using double hits and accent groupings
(defn inorder-bonds [sol accents subst]


  (let [groupings (interpretations sol accents)]

    (symm-bonds (cons sol groupings))

    )

  )
