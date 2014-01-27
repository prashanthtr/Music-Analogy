;Objects in the musicat workspace

;raw input that does not change

(def lead-cycle

  (def diction)
  (def loudness)
  (def timesignature)
  (def intonation)
  )

; Workspace objects/structures that are built by accoompani-cat

(def-ob measures [diction loudness intonation timesignature]

  (
   start  ; absolute start beat of the measure, E.g 2nd measure starts at 5th beat in 4/4 TS
   end    ;absolute end beat of the measure, 2nd measure end at 8th beat in 4/4 TS]
   diction-map
   loudness
   measure-number ;Measure number in the Improvisation cycle, (1 to 8)
   )

  )

; Measure link is formed between any 2 measures in the improvisation cycle
(def-ob measure-links

  (
   from-obj ; Measure number
   to-obj ; Reference to  measure number
   strength ; 0 - 100 (function [from-obj, to-obj]
   )
  )

;Codelets

(defn measure-linker [measure1 measure2]

  (linked-object)

  )

; Takes in m1, m2 and all links involving the 2. Breaks the links that are weak.
(defn measure-breaker [measure1 measure2 common-links]

  )


; Groups and sequences
; Codelets: large pitch leaps, rhythmic gaps, and places where note density changes rapidly to form group boundaries

; In the competeting case of alternate being more identical than adjacent structures, a larger coherent view of the improv cycle is needed to say why the alternate structures will not work. A new codelet is meeded for that. 1. Alternate groups are given less strength than adjacent 2. Alternate are given even less and adjacent more if a similarity mapping is possible with. For example,  1-3 and 2-4 are identical. 1-2 and 3-4 are similar. If 1-2 and 3-4 on the whole are identical then that is preferred.

(def-ob grouping

  (
   list-obj ; contains the list of adjacent measures/groups that form the group
   group-category ; Whether object is a group, sequence
   group-reasons ; Proximity reasons, rhythmic similarity, Loudness, note activity, (alternate similarity -> For example,  1-3 and 2-4 are identical. 1-2 and 3-4 are similar. If 1-2 and 3-4 on the whole are identical then that is preferred.)

      ;The description-types which are the reasons upon which the group is formed (i.e., measure-links, loudness-contour, note-activity length). Idea-> Group codelets that scans alternate, adjacent measures to form groups. Key-> I don't need groups for prediction

   strength ; Strength of the group which is based on the reasons the group exists
   )
  )

;Codelets

(defn group-breaker)

(defn proximity-grouper [from-obj ;measures
                         to-obj
                         ]
  (grouping-obj) ;group reason based on proximity
  )

(defn measure-sameness [obj-1
                        obj-2
                        facet ; measure-link, loudness contour, note activity
                        ]
  (grouping) ; with group reason based on the facet
  )

(defn-ob relationship

  (
   from-obj ; Objects can be measures, groups, meta-groups etc
   to-obj
   relationship-type ; analogy, identical, similar, first identical, last identical, loudness-contour, note-activity (future: melody-contour)
   bond-facet ;E.g. Facet of measures used to form relationship. In the case of groups, group reasons other than proximity is used E.g. Identical similar are formed by Measure-link, Note activity is formed by Note activity, loudness-contour.
   bond-descriptions ; Descriptions involving the bonds
                                 ;  making up the group. e.g.
   strength ; Strength of the analogy relationship is a sum of other relationships that come into it and is computed dynamically
   )
  )

;Codelets

; creates any one of the larger relationships
;  (checks if similar/identical already exists and if not, creates and returns a relationship with similar/identical

(defn look-for-relationship [from-obj ;group or measures
                             to-obj
                             measure-links ; existing measure links between obj
                             relationships
                             ]
  (relationship-object)
  )

;looks for a start similar realtinoship and if it does not find one, it creates one
(defn look-for-starting-relationship
  )

; looks for similar melodic contour between groups or measures
(defn look-for-loudness-contour-relationship [from-obj ; groups or measures
                                              to-obj
                                              ]
  (relationship-object) ;with contour relationship
  )

(defn look-for-note-activity-relationship) ; similarily note activity

(defn look-for-analogy-relationship)


(def-ob analogy

  lhs ; left hand side of analogy,
  rhs
  relationship-lists ; LIst of relationships mapping lhs and rhs
  size ;The duration of both the objects involved in the analogy
  completeness-mapping ; 1-1 correspondence between elements of lhs and rhs
  uncompleteness-list ; For each component of the analogy. This is used to guide the discretionary decisions while playing.
  strength ; Component relationships

  )

;Codelets

;Creates an analogy between obj1 and obj2 with the relationship between the 2 objects added to the relationship lists in the resulting analogy object
(defn create-analogy [
                      relationships ;between 2 objects
                      & obj-1 obj-2
                      ]

  (analogy-object) ;posts analogy object in the workspace
  )

;checks for other relationships between the objects and adds that to analogy object
(defn add-new-relationships-to-analogy [obj-1
                                        obj-2
                                        ]

  )

; removes the least strong analogy object from the set of objects
(defn destroy [obj1
               obj2
               analogy-objects
               ]
  (analogy-objects)
  )


; store strong analogies where size > 4 bars in a separate store place
(defn store-strong-analogies [
                              analogy-obj
                              ]

  )

; retrieves a strong analogy, used for between improvisation cycle comparison
; Question: when to call this function, at the beginning of each new improv cycle, and test the strength and decide whether to use or discard it
(defn resustiate-strong-analogies [
                                   strong-analogy-list
                                   ]

  (analogy-object)
  )



;; To do

; functions to calculate structure strengths
; graphics
; Figure out how to implement objects in clojure
; Answer.l to find how to make modifications to the answer
