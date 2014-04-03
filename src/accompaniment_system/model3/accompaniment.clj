; Takes input in the form of "replace beatNo ____ by ____ " and replaces the corresponding beat number in the secondary.


(defn T [descriptor no replacement a1]

  ;(println "beatno " no " replacement " replacement)
  (assoc a1 no replacement)
  )


(defn durSym [dur]

  (cond

   (= dur 3/4) 'HQ
   (= dur 1/2) 'H
   (= dur 1/4) 'Q
   (= dur 1/8) 'EE

   )

  )

(defn operation [dur op]

  (let [
        dur-val (symDur dur)
        modified (* dur-val op)
        m-dur (durSym modified)
        ]
    (cond

     ; choices of substitution would come here
     (and (= dur 'H) (= m-dur 'Q)) '(Q Q)
     (and (or (= dur 'HQ) (= dur 'QH)) (= m-dur 'Q)) '(Q Q Q)
     :else m-dur

     )
    )

  )

; Translation for note duration

(defn T-R [descriptor no relation a1]

  (let [
        dur (nth a1 no)
        replacement (operation dur relation)
        ]
    (assoc a1 no replacement)
    )

  )

(defn eq-composite [s1 s2]

  (cond

   (= s1 s2) true
   (and  (= s1 'H) (= s2 '(Q Q)) ) true
   (and  (= s1 'HQ) (= s2 '(H Q)) ) true
   (and  (= s1 'HQ) (= s2 '(Q H)) ) true
   (and  (= s2 'H) (= s2 '(Q Q)) ) true
   (and  (= s2 'HQ) (= s2 '(H Q)) ) true
   (and  (= s2 'HQ) (= s2 '(Q H)) ) true
   :else false

   )
  )

(defn T-R-tweak [descriptor group no relation a1]

  (let [
        dur (nth a1 no)
        replacement     (cond
                         (eq-composite group (nth a1 no)) (operation dur relation)
                         :else nil ;see if it is possible to form that bond
                         )
        ]
    ;(println "repl" group replacement)
    (assoc a1 no replacement)
    )

  )




;Takes in input and generates non-relational transformation

(defn generate-A2 [transform a1]

  ;(println "Generate")
  (cond

     ;prototype - "replace beat Number 2 (Q) by (multiply 2)"
   (not= nil (re-find #"\S+ beat Number \d+ \S+ by (\S+\s\d\S+)" transform))
   (let [

         t (re-find #"\S+ beat Number \d+ (\S+) by (\S+\s\d\S+)" transform)
         ;group (read-string (second t))
         multiplier (last (read-string (last t)))
         beat-num (read-string (re-find #"\d+" transform))
         temp-a2 (T-R 'noteDuration (dec beat-num) multiplier (into [] a1))
         ]
     ;(println "here ")
     (nth temp-a2 (dec beat-num))
     )

   ;prototype - "replace beat Number 2 (Q Q) by (multiply 2)"
   (not= nil (re-find #"\S+ beat Number \d+ (\S+\s\S+) by (\S+\s\d\S+)" transform))
   (let [

         t (re-find #"\S+ beat Number \d+ (\S+\s\S+) by (\S+\s\d\S+)" transform)
         group (read-string (second t))
         multiplier (last (read-string (last t)))
         beat-num (read-string (re-find #"\d+" transform))
         temp-a2 (T-R-tweak 'noteDuration group (dec beat-num) multiplier (into [] a1))
         ]
     ;(println "group is" (nth temp-a2 (dec beat-num)))
     (nth temp-a2 (dec beat-num))
     )


   (not= nil (re-find #"\S+ beat Number \d+ by (\S+\s\S+)" transform))
   (let [
         ct (read-string (last (re-find #"\S+ beat Number \d+ by (\S+\s\S+)" transform)))    ]
    ; (println ct)
                                        ;(T 'diction (dec (read-string (re-find #"\d+" transform))) ct (into [] a1) )
     ct
     )

   ; \S+ could be non-relational or relational applied on beats
   (not= nil (re-find #"\S+ beat Number \d+ \S+ \S+" transform))
   (let [
         ct (map symbol (split transform #"\s"))
         ]
     ;(T 'diction (dec (read-string (re-find #"\d" transform))) (last ct) (into [] a1) )
     (last ct)
     )
   :else nil
   )
  )


; Function that parses a set of transformations contained as an input list and applies them on the A1 one by one

(defn list-transforms [list-T a1]

  (let [
        after-T  (map

                   (fn [i]

                     (cond

                      (integer? i) 0
                      :else (generate-A2 i a1)

                      )

                     )
                   list-T)
        a2 (map

                (fn [d1 d2]

                  (cond

                   (= d1 0) d2
                   :else d1
                   )

                  )
                after-T a1)

        ]
    a2
    )

  )

; Each symbol corresponds to a sepcific data type. And, rules are selected based on the data type they belong to.

; THere are 2 types of rules, relational and non-realtional. In non-relational, the output is in the form of diction itself, and is the final output. IN relational, the output is in an abstract form, that needs to be mapped on to the final output in form of diction.
