There are 2 kinds of transforms ,
Relational "replace beat No 3 by ta", "replace beat No 3 by Q"
non- relational  "replace beat No 3 Q by (multiply 1/2)" "replace beat No 3 sameness by difference"

Generating transforms

Transforms are generated by comparing changed objects between i-string and m-string.
changed objects -> diction, note duration, hit-relations (sameness,difference) - note duration relation (sameness,difference)

The way copycat generates the transform is, represents both strings and sees if it is possible to represent any one of the target string objects in relation to an initial string object through a known operation (successorship, predecessorship etc)

abc -> abd
abc -> ab "successor of c(letter cateogry)" / "successor of rightmost (string-position" /

But since this kind of relation is not possible between the different hits of the secondary, and lead and secondary play different hits on their instruments, there is a need for a more abstract representation using which relations between hits are possible. This is through note duration.

ta tum tum ta -> ta tum tum (tate)
ta tum tum ta -> ta tum tum "replace ta by (ta te)"
ta tum tum ta -> ta tum tum "replace beat No 4 by (tate)"
Q  Q   Q   Q  -> Q  Q   Q   "multiply beat No 4 by 1/2"
   d   s   d  ->    d   s   ""  -- in bonds diction
d  s   d   s  -> d  s   d   ""  -- out bonds diction
   s   s   s  ->    s   s   "replace beat No 4 inbond by Diff"  -- in bonds noteduration
d  s   d   s  -> d  s   d   "replace beat No 3 outbond by Diff" -- out bonds noteduration

There could groups like H, HQ etc. But these groups have group reasons of sameness or pitch bend. So, the changes could be along those dimensions as well.

ta tum tum ta -> ta tum .   ta
   d   s   d  ->    d   pb  d  "replace beat No 3 inbond by pb"  -- in bonds diction
d  s   d   s  -> d  pb  d      "replace beat No 2 outbond by pb"  -- out bonds diction

The shift to higher level representation for generating transforms:

In copycat, the shift to a different descriptor-type of the string objects is done when there is more than 1 object change between the initial and modified string.

However, from my experience and based on observations, even novice secnodary percussionists are able to accomodate 3/4 changes between l1 and l2 and transform their accompaniment. The motivation for higher level (Relational) transform could be prompted by generation of successive lower level (NR) transformations of 3 or 4 changes.

At the note duration level, however, if there are inconsistent patterns, then an even higher level of groupings can be percieved. The note durations consistently repeat on a grouping different than 4 or 8 groupings. Then, the analogies can be percieved amongst those groups rather than amongst 4 beats. Which means that at any given point, there are 4,8, and other kinds of groupings possible. The group that is strongest or most consistent is kept for making analogies.

Criteria for inconsistent repetiton is more than 75% of the group hits changes in the next iteration.

\begin {enumerate}
\item non-relational using diction
    \subitem inconsistent repetition
       \subsubitem note groupings using diction
           \subitem consistent repetition
              \subsubsubitem non-relational using diction
           subitem inconsistent repetition
                relational using note duration
                   consistent
                     apply relational transformation
                   inconsistent repetition
                     change note groupings using note duration --
                         consistent repetition
                          relational using note duration
                         inconsistent repetition
                          pause and join

Representing tranforms

Transforms are represented as the "replace beat Number ____ by _____" (or)
Transforms are represented as the "replace beat Number ____ descriptor type ____ by relation _____"

The system recognizes the type of transform as a Regular expression, extracts the correspondng symbols, namely beatNo, descriptor type and transformation action and applies on A1.

applying transforms

Things to do:

1. retrieve relevant descriptions of string given a descriptor type
list of valid descriptions is stored in a bigger object - string object

store in-bonds
store outbonds bonds
store inner-bonds
generate external bonds for changed object

use external bond to create the transformation

2. use valid descriptions to generate and apply transformations
3. selecting/deciding diction from abstract transformations

4. Grouping of strings for analogy making


Overall framework is to have strings represent transformations
and RE that matches the string decipher the transformations to apply

overall framework is to have  object descriptions of objects that can be compared to get a relation/non-relational transform



Transforms are represented as the "replace beat Number ____ by _____" (or)
Transforms are represented as the "replace beat Number ____ descriptor type ____ by relation _____"

(def pr

  '{
   (Q Q)
   {{ta ta ta te ta tum ta .}
   {te ta te te te tum te .}
   {tum ta tum te tum tum tum .}
   {. ta . te . tum . .}}
   }


  (Q EE)
  {{ta (ta te) ta (te ta) ta (tum ta) ta (tum te)}
   {te (ta te) te (te ta) te (tum ta) te (tum te)}
   {tum (ta te) tum (te ta) tum (tum ta) tum (tum te)}
   {. (ta te) . (te ta) . (tum ta) . (tum te)}
   }

  ('(Q Q) H)
  {{(ta ta) ta (ta .) ta}
   {(te te) te (ta .) te}
   {(tum tum) ta (tum .) tum}
   {(. .) .}

   }

  )



non relational
function that takes in a descriptor like "replace ta by (ta te)"
replaces ta by tate and end of story

relational
function that takes in a descriptor like "multiply Q by 1/2"
computes that Q * 1/2 is EE
matching case: Q -> EE



(defn find-pr [sym1 sym2]

  (get pr sym1)


  )