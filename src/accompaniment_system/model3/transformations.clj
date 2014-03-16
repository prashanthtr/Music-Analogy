;; This file contains the data structures and methods needed to obtain transformations between the workspace representations of L1 and L2.


(defprotocol Transform

  (transform [this] "Transform")

  )



(defrecord workspace-obj [feature ;note duration, note quality
                           type ; value, relation
                           beatNo ; numbers 1,2,3 or lists (1 2), (1 3), (1 2 3) etc
                           value ;Q,H,EE, sameness, difference
                          ; representations ; Representations of L1 and L2
                          ; objects ; L1 and L2
                           ]
  Transform
(transform [this] ( str "change " (:feature this) " of type " (:type this) " with value " (:value this) " on beat Number " (:beatNo this) " to"))

)



(defmulti gen-transform
  (fn [x] (x "type")))

(defmethod gen-transform "value" [params]
  "Q,H,EE")

(defmethod gen-transform "relation" [params]
  "sameness/difference!")


(def l1 {"type" "value" 'value 'EE 'relation nil})
