(* SYNTAX OF RECHML                               *)  
(* tt                  truth                      *)
(* ff                  falsehodd                  *)
(* f1 or f2            disjunction                *)
(* f2 and f2           conjunction                *)
(* <a>f                existential modality       *)
(* [a]f                universal modality         *)
(* min X.f             least fixpoint             *)
(* max X.f             greatest fixpoint          *)
(* X                   recursion variable         *)

type action = string
type variable = string

type formula =
  | TT
  | FF
  | Disjunction of formula * formula
  | Conjunction of formula * formula
  | Existential of action * formula
  | Universal of action * formula
  | Min of var * formula
  | Max of var * formula
  | Var of variable
