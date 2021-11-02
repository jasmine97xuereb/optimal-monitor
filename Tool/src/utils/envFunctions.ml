open PrettyPrint
open EnvResources

(* Init a reference to empty map *)
let map = ref LVars.empty

(* Function that takes a formula and attempts to simplify it using the Laws of Boolean Algebra *)
(* We only check for two laws: Annulment Law & Identity Law                                    *)
(* Annulment Law: A and false = false, A or true = true                                        *)
(* Identity Law: A and true = true, A or false = A                                             *)

let rec simplify (f: Ast.formula): Ast.formula = 
  match f with 
  | TT | FF | LVar _ -> f
  | Existential(a, cont) -> Existential(a, simplify cont)
  | Universal(a, cont) -> Universal(a, simplify cont)
  | Min(x, cont) -> Min(x, simplify cont)
  | Max(x, cont) -> Max(x,simplify cont)
  | Disjunction(l, r) -> let l_smp = simplify l in 
                          begin
                          match l_smp with
                          | TT -> TT
                          | FF -> simplify r
                          | _  -> let r_smp = simplify r in
                                  match r_smp with
                                  | TT -> TT
                                  | FF -> l_smp
                                  | _ -> Disjunction(l_smp,r_smp)
                          end                     
  | Conjunction(l, r) -> let l_smp = simplify l in 
                          begin
                          match l_smp with
                          | TT -> simplify r
                          | FF -> FF
                          | _  -> let r_smp = simplify r in
                                  match r_smp with
                                  | TT -> l_smp
                                  | FF -> FF
                                  | _ -> Conjunction(l_smp, r_smp)
                          end

(* Function to traverse the ast of a formula and add entries to map: LVar -> Formula *)
let rec populate_map (f: Ast.formula): unit = 
  match f with
  | TT | FF | LVar _ -> ()
  | Disjunction(l, r) -> populate_map l; populate_map r
  | Conjunction(l, r) -> populate_map l; populate_map r 
  | Existential(a, cont) -> populate_map cont 
  | Universal(a, cont) -> populate_map cont 
  | Min(x, cont) -> map := LVars.add x f !map; populate_map cont
  | Max(x, cont) -> map := LVars.add x f !map; populate_map cont

(* Function to traverse the ast of a formula and add entries to map: LVar -> Formula *)
let rec update_map (f: Ast.formula): unit = 
  match f with
  | TT | FF | LVar _ -> ()
  | Disjunction(l, r) -> update_map l; update_map r
  | Conjunction(l, r) -> update_map l; update_map r 
  | Existential(a, cont) -> update_map cont 
  | Universal(a, cont) -> update_map cont 
  | Min(x, cont) -> map := LVars.update x (fun _ -> Some f) !map; update_map cont
  | Max(x, cont) -> map := LVars.update x (fun _ -> Some f) !map; update_map cont
