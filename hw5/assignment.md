---
title: 161 Homework 5
author: Rodrigo Valle
date: November 9, 2017
geometry: margin=0.5in
---

# Problem 1

## Part A

 - $P \Rightarrow \neg Q$
     * $(\neg P \lor \neg Q)$

 - $Q \Rightarrow \neg P$
     * $(\neg Q \lor \neg P)$

-----------------------------------------------------------------------------------
 $P$   $Q$   $\neg P$   $\neg Q$   $P \Rightarrow \neg Q$   $Q \Rightarrow \neg P$
----- ----- ---------- ---------- ------------------------ ------------------------
  F     F       T          T                 T                        T

  F     T       T          F                 T                        T

  T     F       F          T                 T                        T

  T     T       F          F                 F                        F

-----------------------------------------------------------------------------------

## Part B

 - $P \iff \neg Q$
     * $(P \Rightarrow \neg Q) \land (\neg Q \Rightarrow P)$
     * $(\neg P \lor \neg Q) \land (Q \lor P)$

 - $((P \land \neg Q) \lor (\neg P \land Q))$

------------------------------------------------------------------------------------------------
 $P$   $Q$   $\neg P$   $\neg Q$   $P \iff \neg Q$   $((P \land \neg Q) \lor (\neg P \land Q))$
----- ----- ---------- ---------- ----------------- --------------------------------------------
  F     F       T          T              F                           F

  F     T       T          F              T                           T

  T     F       F          T              T                           T

  T     T       F          F              F                           F

------------------------------------------------------------------------------------------------

\pagebreak

# Problem 2

## Part A

Let $F = \text{Fire}$, $S = \text{Smoke}$, and $H = \text{Heat}$.

 - $(S \Rightarrow F) \Rightarrow (\neg S \Rightarrow \neg F)$

 - $S \Rightarrow F$
     * $\neg S \lor F$

 - $\neg S \Rightarrow \neg F$
     * $S \lor \neg F$

-----------------------------------------------------------------------------------------------------------------------------------------------------
 $S$   $F$   $\neg S$   $\neg F$   $\neg S \lor F$   $S \lor \neg F$   $(S \Rightarrow F) \Rightarrow (\neg S \Rightarrow \neg F)$
----- ----- ---------- ---------- ----------------- ----------------- -------------------------------------------------------------
  F     F       T          T                T               T                     T

  F     T       T          F                T               F                     F **

  T     F       F          T                F               T                     T

  T     T       F          F                T               T                     T

-----------------------------------------------------------------------------------------------------------------------------------------------------

Neither valid or unsatisfiable, relationship does not hold under all possible
inputs (but is not universally unsatisfiable). See double-starred (**) for a
counterexample (if the left side of an implication is true, the right side must
be true as well).

## Part B

 - $(S \Rightarrow F) \Rightarrow ((S \lor H) \Rightarrow F)$

 - $(S \Rightarrow F)$
     * $\neg S \lor F$

 - $((S \lor H) \Rightarrow F)$
     * $\neg (S \lor H) \lor F$
     * $(\neg S \land \neg H) \lor F$

----------------------------------------------------------------------------------------------------------------------------------------------------------------
 $S$   $F$   $H$   $\neg S$   $\neg F$   $\neg H$   $\neg S \lor F$   $(\neg S \land \neg H) \lor F$   $(S \Rightarrow F) \Rightarrow ((S \lor H) \Rightarrow F)$
----- ----- ----- ---------- ---------- ---------- ----------------- -------------------------------- ------------------------------------------------------------
  F     F     F       T          T          T               T                        T                                            T

  F     F     T       T          T          F               T                        F                                            F **

  F     T     F       T          F          T               T                        T                                            T

  F     T     T       T          F          F               T                        T                                            T

  T     F     F       F          T          T               F                        F                                            T

  T     F     T       F          T          F               F                        F                                            T

  T     T     F       F          F          T               T                        T                                            T

  T     T     T       F          F          F               T                        T                                            T

----------------------------------------------------------------------------------------------------------------------------------------------------------------

Neither valid or unsatisfiable, the relationship does not hold under all
possible inputs, see the double-starred (**) row.

## Part C

 - $((S \land H) \Rightarrow F) \iff ((S \Rightarrow F) \lor (H \Rightarrow F))$
    * $(\neg (S \land H) \lor F) \iff ((\neg S \lor F) \lor (\neg H \lor F))$
    * $((\neg S \lor \neg H) \lor F) \iff ((\neg S \lor F) \lor (\neg H \lor F))$

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 $S$   $F$   $H$   $\neg S$   $\neg F$   $\neg H$   $(\neg S \lor \neg H) \lor F$   $(\neg S \lor F) \lor (\neg H \lor F)$   $((S \land H) \Rightarrow F) \iff \newline ((S \Rightarrow F) \lor (H \Rightarrow F))$
----- ----- ----- ---------- ---------- ---------- ------------------------------- ---------------------------------------- ----------------------------------------------------------------------------------------
  F     F     F       T          T          T                     T                                     T                                                  T

  F     F     T       T          T          F                     T                                     T                                                  T

  F     T     F       T          F          T                     T                                     T                                                  T

  F     T     T       T          F          F                     T                                     T                                                  T

  T     F     F       F          T          T                     T                                     T                                                  T

  T     F     T       F          T          F                     F                                     F                                                  T

  T     T     F       F          F          T                     T                                     T                                                  T

  T     T     T       F          F          F                     T                                     T                                                  T

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Valid, both sides of the if and only if always result in the same value.

\pagebreak

# Problem 3

Let $M = \text{Mythical}, I = \text{Immortal}, H = \text{Horned}, Y = \text{Magical}$, and $Z = \text{Mammal}$

## Part 1: Knowledge Base

------------------------------------------------------------------------------------------------------------
English                                                                Propositional sentence
---------------------------------------------------------------------- -------------------------------------
*If the unicorn is mythical, then it is immortal*                      $M \Rightarrow I$

*but if it is not mythical, then it is a mortal mammal*                $\neg M \Rightarrow (\neg I \land Z)$

*If the unicorn is either a immortal or a mammal, then it is horned*   $(I \lor Z) \Rightarrow H$

*The unicorn is magical if it is horned*                               $H \Rightarrow Y$

------------------------------------------------------------------------------------------------------------

## Part 2: Convert to CNF

--------------------------------------------------------------------------------------------------------------
Propositional sentence                  Sentence                       CNF
--------------------------------------  ------------------------------ ---------------------------------------
$M \Rightarrow I$                       $\neg M \lor I$                $\neg M \lor I$

$\neg M \Rightarrow (\neg I \land Z)$   $M \lor (\neg I \land Z)$      $(M \lor \neg I) \land (M \lor Z)$

$(I \lor Z) \Rightarrow H$              $(\neg I \land \neg Z) \lor H$ $(\neg I \lor H) \land (\neg Z \lor H)$

$H \Rightarrow Y$                       $\neg H \lor Y$                $\neg H \lor Y$

--------------------------------------------------------------------------------------------------------------

These rules are shared between each part of Part 3:

-----------------------
 Rule   CNF
------  ---------------
  1     $\neg M \lor I$

  2     $M \lor \neg I$

  3     $M \lor Z$

  4     $\neg I \lor H$

  5     $\neg Z \lor H$

  6     $\neg H \lor Y$

-----------------------


## Part 3

### Is the unicorn mythical?

-------------------------------------------------------------------------
 Rule   CNF                            Combining Rules
------  ------------------------------ ----------------------------------
  7     $\neg M$                       Assume the unicorn is not mythical

  8     $I \lor Z$                     1, 3

  9     $Z \lor H$                     8, 4

  10    $H$                            9, 5

  11    $Y$                            10, 6

  12    $Z$                            7, 3

-------------------------------------------------------------------------

It cannot be shown from the knowledge base that the unicorn is mythical.

### Is the unicorn magical?

-------------------------------------------------------------------------
 Rule   CNF                            Combining Rules
------  ------------------------------ ----------------------------------
  7     $\neg Y$                       Assume the unicorn is not magical

  8     $I \lor Z$                     1, 3

  9     $Z \lor H$                     8, 4

  10    $H$                            9, 5

  11    $Y$                            10, 6

  12    FALSE                          11, 7

-------------------------------------------------------------------------

We conclude that the unicorn is magical.

### Is the unicorn horned?

-------------------------------------------------------------------------
 Rule   CNF                            Combining Rules
------  ------------------------------ ----------------------------------
  7     $\neg H$                       Assume the unicorn is not horned

  8     $I \lor Z$                     1, 3

  9     $Z \lor H$                     8, 4

  10    $H$                            9, 5

  11    FALSE                          10, 7

-------------------------------------------------------------------------

We conclude that the unicorn is horned.
