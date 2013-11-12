;The file contains the different system versions that select and play accompaniment with different levels of discretionary improvisational ability

;; algo,
;; first bar -> generate exact lead
;; second -> substitute with improv choices at the variable position
;; third -> substitute with improv choices at the variable position
;; fourth -> substitute with improv choices at the variable position

;; one version of the system that strictly plays only with the forced choices

;; one version that generates substitutions only at the pauses and double hits, when no pauses and double hits, it follows the lead exactly

;; other version, that generates substitutions only at the pauses and double hits, when no pauses and double hits, that introduces random variable note at any position(1st and last position only), generates substitutions and follows

;; other version that substitutes at pauses, double hits and also at the non accented positions, selection of choices is random.

;;only one thing to calibrate -> loudness is same as perceptual loudnes, hmm


(load-file "src/accompaniment_system/generate.clj")
(load-file "src/accompaniment_system/select.clj")
(load-file "src/accompaniment_system/sound.clj")

(defn systemv1 [sol]

  (println (apply-rule-map sol ))
  ;(apply-rule-map (mriMap mridangam) )
  ;mridangam
  (list (apply-rule-map (mriMap sol) ))

  )

(defn hashToList [hash]

  (cond
   (empty? hash) nil
   :else (cons (first hash) (hashToList (rest hash)))

   )

  )

(defn systemv2 [mridangam]

  (let [ pos (positions mridangam) mri (main mridangam (list (hashToList (clojure.set/difference (set '(0 1 2 3 4 5 6 7)) (set pos) )))  {'. '?p '(ta te) '?d '(te ta) '?d '(ta tum) '?d '(tum tum) '?d '(tum ta) '?d '(tum te) '?d 'tum '?nA 'ta '?nA 'te '?nA} ) ]

    (display mri)
    ;mri
    )

 )

(defn systemv3 [mridangam]

  (let [ pos (positions mridangam) mri (main mridangam (list (random-subst (combinations '(0 1 2 3 4 5 6 7) 2) 0)) {'. '?p '(ta te) '?d '(te ta) '?d '(ta tum) '?d '(tum tum) '?d '(tum ta) '?d '(tum te) '?d 'tum '?nA 'ta '?nA 'te '?nA} ) ]
    (println mri)
    mri
    )

  )

;;selects from amongst the choices and plays only the compatible ones as per the above loudness conditions

(defn systemv4 [mridangam loudness]


  (let [ pos (positions mridangam) mri (main mridangam (list (random-subst (combinations '(0 1 2 3 4 5 6 7) 2) 0)) {'. '?p '(ta te) '?d '(te ta) '?d '(ta tum) '?d '(tum tum) '?d '(tum ta) '?d '(tum te) '?d 'tum '?nA 'ta '?nA 'te '?nA} ) ]

                                        ;(concat (combinations '(0 1 2 3 4 5 6 7) 2) (combinations '(0 1 2 3 4 5 6 7) 1) (combinations '(0 1 2 3 4 5 6 7) 3))
   ; comb (list (random-subst (combinations '(0 1 2 3 4 5 6 7) 2) 0))

    (println (mriMap mridangam) loudness )
    (find-differ (mriMap mridangam) variations loudness )

    ;(display (distinct mri))
    ;mri
    )

  )
