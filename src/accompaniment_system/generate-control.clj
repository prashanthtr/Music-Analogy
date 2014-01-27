;;  Bit of code generates accompaniment based on an abstract representation. The following ddetails are hardcoded into the code.

;; . -> 0
;; ri -> 1
;; ta -> 2
;; te,tum -> 3,5
;; tumki -> 6
;; tate -> 4

;; Loudness -> 0 to 3

;The loudness values are in steps of 0.25. Given an abstract description, the task is to figue out the sequences of hits that will match the description.

;Abstract description -> u-d-U-D-.
;The first note is with respect to the level 0.75. The subsequent notes are with respect to the first description. loudness within the range of +- 0.25 is classified under the same loudness range. The maximum that loudness can add (range of x) is 0.75(3). And loudness increases or decrases in units of 0.25


(defn fun [x]

  (let [

        n1 (rand-int (- x 1))
        n2 (rand-int 4)
        ]
    (cond

     (= x (+ n1 n2)) (list n1 n2)
     :else (fun x)
     )

    )

  )

(defn hits [hit-map x]

  (let [
        hits (fun x)
        h1 (first hits)
        h2 (last hits)
        hit (get (clojure.set/map-invert hit-map) h1)
        ]
    ;(println hit h2)
    hit
    )

  )

(defn map-symbol-num [pre sym]

  (cond

   (= sym '-) 0
   (= sym 'u) (+ (rand-int 2) 1)
   (= sym 'd) (- 2 (rand-int 2))
   (= sym 'D) (- 0 (rand-int (+ pre 1)) )
   (= sym 'U) (+ 0 (rand-int pre) )
   )

  )

;; u-U-D-d-
(defn possible-interpret-contour [previ contour]

  (cond

   (empty? contour) nil
   :else (let [
               n1 (+ previ (map-symbol-num previ (first contour)))
               ]

           (cons n1 (possible-interpret-contour n1 (rest contour) ))
           )
   )
  )

(defn abstract-rep [contour hit-map]

  (let [

        interpret (possible-interpret-contour 3 contour)
        diction (map (partial hits hit-map) interpret)
        ]
    diction
    )

  )
