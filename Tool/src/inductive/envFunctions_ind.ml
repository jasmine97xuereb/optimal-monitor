open PrettyPrint

(* Function that takes a formula and attempts to simplify it using the Laws of Boolean Algebra *)
(* We only check for two laws: Annulment Law & Identity Law                                    *)
(* Annulment Law: A and false = false, A or true = true                                        *)
(* Identity Law: A and true = true, A or false = A                                             *)

let rec simplify = fun
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
