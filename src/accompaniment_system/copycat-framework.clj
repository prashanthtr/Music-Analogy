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

;;function returns description of the position of the double hit in the beginning, middle and end of the pattern
;beginning 1 beat - first
;middle 2 beats - (first last)
;end last


;;builds description of double hits in the pattern

(defn doublehit [sol bindings var]

  (let [
        b1 (cond

            (list? (first sol)) (merge bindings {(cons '(first) var) (first sol) })
            :else bindings
            )
        b2 (cond

            (list? (last sol)) (merge b1 {(cons '(last) var) (last sol) })
            :else bindings
            )
        len (lengthList sol 0)
        halflen (/ len 2)
        mid (subsequ sol (- halflen 1) halflen 0)
        middle '(middle)
        b3 (cond

            (and (list? (first mid)) (list? (first sol)) ) (merge b2 {(cons (cons 'whole middle) var) mid})
            (list? (first mid)) (merge b2 {(cons (cons '(first) middle) var) (first mid) })
            (list? (last mid)) (merge b2 { (cons (cons '(last) middle) var) (last mid)})
            :else b2
            )

        ]
    b3
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
        b1 (doublehit sol bindings '(dh whole))
        b2 (doublehit firsthalf b1 '(dh first))
        b5 (doublehit secondhalf b2 '(dh second))
        ]
    (merge b3 b4 b5)
    )

  )

; checks and creates the symmetry bindings if any for each of the list
(defn symm-bonds [lists]

  (cond

   (empty? lists) nil
   :else  (cons (sym-sol (first lists) {}) (symm-bonds (rest lists)) )
   )

  )



(defn desc-matching [ sol1-desc sol2-desc]

  (cond

   (empty? sol1-desc) nil
   :else (let [
               desc (nth (first sol1-desc) 0)
               ]
           (cond

            (not= nil (get sol2-desc desc)) (do (println "bridge formed" desc) (desc-matching (rest sol1-desc) sol2-desc) )
            :else (desc-matching (rest sol1-desc) sol2-desc)
            )

           )
   )

  )

;creates bridges between descriptor notatios
(defn bridges [sol]

  (let [
        descriptors (symm-bonds sol)
        desc1 (first descriptors)
        desc2 (last descriptors)
        ]
    (desc-matching desc1 desc2)

    )

  )