module B = PrintBox
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
  | Min of variable * formula
  | Max of variable * formula
  | Var of variable

let rec simplify = function
                 | Var _ | TT | FF as f -> f
                 | Existential(a,f) -> Existential(a,simplify f)
                 | Universal(a,f) -> Existential(a,simplify f)
                 | Min(x,f) -> Min(x,simplify f)
                 | Max(x,f) -> Min(x,simplify f)
                 | Disjunction(f1,f2) -> let g1 = simplify f1 in
                                         begin
                                         match g1 with
                                         | TT -> TT
                                         | FF -> simplify f2
                                         | _  -> let g2 = simplify f2 in
                                                 match g2 with
                                                 | TT -> TT
                                                 | FF -> g1
                                                 | _ -> Disjunction(g1,g2)
                                         end
                 | Conjunction(f1,f2) -> let g1 = simplify f1 in
                                         begin
                                           match g1 with
                                           | TT -> simplify f2
                                           | FF -> FF
                                           | _  -> let g2 = simplify f2 in
                                                   match g2 with
                                                   | TT -> g1
                                                   | FF -> FF
                                                   | _ -> Conjunction(g1,g2)
                                         end
;;

(* Print the recHML formula *)

let rec formula_to_box = function
                   | TT -> B.tree (B.text "tt") []
                   | FF -> B.tree (B.text "ff") []
                   | Disjunction(f1,f2) -> B.tree (B.text "OR") [formula_to_box f1;formula_to_box f2]
                   | Conjunction(f1,f2) -> B.tree (B.text "AND") [formula_to_box f1;formula_to_box f2]
                   | Existential(a,f) -> B.tree (B.text ("<"^a^">")) [formula_to_box f]
                   | Universal(a,f) -> B.tree (B.text ("["^a^"]")) [formula_to_box f]
                   | Min(x,f) -> B.tree (B.text ("Min "^x)) [formula_to_box f]
                   | Max(x,f) -> B.tree (B.text ("Max "^x)) [formula_to_box f]
                   | Var(x) -> B.tree (B.text x) []
;;

let pretty_print_ast f  = PrintBox_text.output stdout (formula_to_box f);;

let f = Disjunction(FF,Conjunction(Conjunction(Var("x"),TT),TT)) in
  pretty_print_ast f;
  print_endline("\n");
  pretty_print_ast (simplify f)
