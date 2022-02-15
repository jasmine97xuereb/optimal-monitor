open Ast
open EnvFunctions

(* This function is only correct for formulas in disjunctive normal form. *)
let satisfiable formula =
  let rec aux_sat nuvar = function
    | TT -> true
    | FF -> false
    | Disjunction(phi,psi) -> aux_sat nuvar phi || aux_sat nuvar psi
    | Conjunction(phi,psi) -> aux_sat nuvar phi && aux_sat nuvar psi
    | Existential(a,phi) -> aux_sat nuvar phi
    | Universal(a,phi) -> true
    | LVar(X) -> mem X nuvar
    | Min(X,phi) ->  aux_sat nuvar phi
    | Max(X,phi) -> aux_sat (X::nuvar) phi
  in
  aux_sat [] formula
