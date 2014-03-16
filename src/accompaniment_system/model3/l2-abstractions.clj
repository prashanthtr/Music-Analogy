;; Creates L2 abstractions from the Literal abstraction.

;; This is where the choices in terms of production rules are made

;Q1 Q2 Q2 Q3 -> Q H Q - using production rule Q2 Q2 -> H
;Q1 Q2 Q2 Q3 -> Q Q Q Q - using prod rule Q1 -> Q
;Q1 Q2 Q2 Q3 -> Q Q H - Using prod rule Q2 Q3 -> H


(defn l2-abstration [diction]

  (let [

        l1-abstr (literal-abstr diction)

        ]
    l1-abstr
    )
  )


; input is literal abstraction
; Output is a series of regular expressions that are tied to the literal abstr
; these expressions further abstracted out to Q notations

;The piece of code creates internal bonds between the different diction elements and relates them as similarity or pitch bend bonds

; input -  ta tum tum .
; output - {s (2 3), p (3 4)} Existence of sameness in diction 2 and 3
;and existence of pitch bend bond between 3 and 4. These will guide the production rules that can be applied on the literal abstraction

(defn sameness [diction assoc]

  (map

   (fn [d1 d2]

     (cond

      (= d1 d2) 's ;(merge assoc {'s '(ind (+ index 1))})
      (and (not= d1 '.) (= d2 '.)) 'p ; (merge assoc {'s '(ind (+ index 1))})
      :else 'd
      )

     )
   diction (rest diction)
   )

  )


(defrecord )
