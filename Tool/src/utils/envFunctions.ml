open EnvResources
open PrettyPrint

(* Init a reference to empty map *)
let map = ref LVars.empty

(* Function that takes a formula and attempts to simplify it using the following axioms:       *)
(* A1. Annulment Law: A and false = false, A or true = true                                    *)
(* A2. Identity Law: A and true = true, A or false = A                                         *)
(* A3. [a]tt = tt                                                                              *)
(* A4. min X. tt = tt                                                                          *)
(* A5. max X. X = tt                                                                           *)
(* A6. max X.[a]X ^ ... ^ [w]X = tt                                                            *)

(* Function to check whether axiom A6 can be applied *)   

let rec axiom (f: Ast.formula) (lvar): bool = 
  match f with 
  | Universal(a, cont) -> 
      if cont = lvar then true 
      else false
  | Conjunction(l, r) -> 
      if axiom l lvar then
      axiom r lvar
      else false  
  | _ -> false
     

let rec simplify (f: Ast.formula): Ast.formula = 
  match f with 
  | TT | FF | LVar _ -> f
  | Existential(a, cont) -> Existential(a, simplify cont)
  | Universal(a, cont) -> (
      let smp = simplify cont in 
      match smp with
      | TT -> TT
      | _ -> Universal(a, smp)
    )
  | Min(x, cont) ->  (
      let smp = simplify cont in 
      match smp with
      | TT -> TT
      | _ -> Min(x, smp)
    )
  | Max(x, cont) -> ( 
      let smp = simplify cont in 
      match smp with
      | LVar x -> TT
      | _ -> 
        if axiom smp (LVar(x)) 
        then TT  
        else Max(x, smp)
    )
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


let rec fv (f: Ast.formula) (free: VarSet.t): VarSet.t = 
  match f with 
  | TT | FF -> free
  | LVar x -> VarSet.add x free
  | Disjunction(l, r) -> VarSet.union (fv l free) (fv r free)
  | Conjunction(l, r) -> VarSet.union (fv l free) (fv r free)
  | Existential(_, cont) -> fv cont free  
  | Universal(_, cont) -> fv cont free 
  | Min(x, cont) -> VarSet.remove x (fv cont free) 
  | Max(x, cont) -> VarSet.remove x (fv cont free)


(* Function to traverse the ast of a formula and add entries to map: LVar -> Formula                                                  *)
(* Return a formula in case there were LVars that were bound multiple times and the function had to perform some variable renaming.   *)

let rec populate_map (f: Ast.formula) (used: VarSet.t): Ast.formula = 
  let rec inner_populate (f: Ast.formula) (used: VarSet.t): Ast.formula * VarSet.t = 
    match f with
    | TT | FF | LVar _ -> (f, used)
    | Disjunction(l, r) -> 
                      let (new_l, used) = inner_populate l used in 
                      let (new_r, used) = inner_populate r used in
                      (Disjunction(new_l, new_r), used)      
    | Conjunction(l, r) -> 
                      let (new_l, used) = inner_populate l used in 
                      let (new_r, used) = inner_populate r used in
                      (Conjunction(new_l, new_r), used)
    | Existential(a, cont) -> (Existential(a, fst (inner_populate cont used)), used)
    | Universal(a, cont) -> (Universal(a, fst (inner_populate cont used)), used) 
    | Min(x, cont) -> if VarSet.mem x used
                      then (
                        let y = fresh used 1 in
                        let (cont, used) = inner_populate (subst cont x y) (VarSet.add y used) in
                        let new_min = Ast.Min(y, cont) in
                        map := LVars.add y cont !map;
                        (new_min, used)
                      )
                      else(
                        let (cont, used) = inner_populate cont (VarSet.add x used) in
                        map := LVars.add x cont !map;
                        (Ast.Min(x, cont), used)
                      ) 
    | Max(x, cont) -> if VarSet.mem x used
                      then (
                        let y = fresh used 1 in
                        let (cont, used) = inner_populate (subst cont x y) (VarSet.add y used) in
                        let new_max = Ast.Max(y, cont) in
                        map := LVars.add y cont !map;
                        (new_max, used)
                      )
                      else(
                        let (cont, used) = inner_populate cont (VarSet.add x used) in
                        map := LVars.add x cont !map;
                        (Ast.Max(x, cont), used)
                      )
in fst (inner_populate f used)

(* Substitute all free occurences of variable x in formula f by variable y. *)
and subst (f: Ast.formula) (x: Ast.variable) (y: Ast.variable): Ast.formula = 
  match f with 
  | TT | FF -> f
  | LVar(z) -> if z = x then LVar(y) else f
  | Disjunction(l, r) -> Disjunction (subst l x y, subst r x y)
  | Conjunction(l, r) -> Conjunction (subst l x y, subst r x y)
  | Existential(a, cont) -> Existential(a, subst cont x y)  
  | Universal(a, cont) -> Universal(a, subst cont x y) 
  | Min(z, cont) -> if z != x then Min(z, subst cont x y) else f 
  | Max(z, cont) -> if z != x then Max(z, subst cont x y) else f

and fresh (free: VarSet.t) (count: int): Ast.variable =
  print_endline("in fresh");
  let v = "X" ^ (string_of_int count) in
  if VarSet.mem v free then fresh free (count+1) else v   

(* Function to traverse the ast of a formula and add entries to map: LVar -> Formula *)
let rec update_map (f: Ast.formula): unit = 
  match f with
  | TT | FF | LVar _ -> ()
  | Disjunction(l, r) -> update_map l; update_map r
  | Conjunction(l, r) -> update_map l; update_map r 
  | Existential(_, cont) -> update_map cont 
  | Universal(_, cont) -> update_map cont 
  | Min(x, cont) -> map := LVars.update x (fun _ -> Some cont) !map; update_map cont
  | Max(x, cont) -> map := LVars.update x (fun _ -> Some cont) !map; update_map cont

let rec disjunction_free (f: Ast.formula): bool = 
  match f with 
  | TT | FF | LVar _ -> true
  | Disjunction _ -> false
  | Conjunction(l, r) -> (disjunction_free l) && (disjunction_free r)
  | Existential(_, cont) -> disjunction_free cont
  | Universal(_, cont) -> disjunction_free cont
  | Min(_, cont) -> disjunction_free cont
  | Max(_, cont) -> disjunction_free cont


let rec tree_size (f: Ast.formula): int = 
  match f with
  | TT | FF | LVar _ -> 1
  | Disjunction (l, r) -> (tree_size l) + (tree_size r) + 1 
  | Conjunction(l, r) -> (tree_size l) + (tree_size r) + 1
  | Existential(_, cont) -> (tree_size cont) + 1
  | Universal(_, cont) -> (tree_size cont) + 1
  | Min(_, cont) -> (tree_size cont) + 1
  | Max(_, cont) -> (tree_size cont) + 1

let rec generate_property_detecter (f: Ast.formula): unit = 
  let rec inner_generate (f: Ast.formula): string = 
    match f with
    | TT | FF | LVar _ -> formula_to_string f
    | Max(x, cont) -> "max " ^ x ^ ". (" ^ (inner_generate cont) ^ ")"   
    | Conjunction _ -> 
      let new_cont = List.map (fun x -> inner_generate x) (get_inner_ands f [])
      in "and(" ^ list_to_string new_cont ^ ")"
    | Universal(a, cont) -> "[" ^ a ^ "]" ^ inner_generate cont
    | _ -> "Error"
  in  
  (* Write to file *)
  let text = inner_generate f in
  print_endline("\nPlease enter the full file path of where you want the output to be written.");
  let file = Scanf.scanf "%s" (fun x -> x) in
  let oc = open_out file in         (* create or truncate file, return channel *)
  Printf.fprintf oc "\n%s\n" text;    (* write something *)   
  print_endline("\nFormula successfully written to file.\n");
  close_out oc                      (* flush and close the channel *)
   

and get_inner_ands (f: Ast.formula) (acc: Ast.formula list): Ast.formula list = 
  match f with 
  | Conjunction(l, r) -> let left = (get_inner_ands l ([l] @ acc)) in (get_inner_ands r (left @ [r]))
  | _ -> acc

and list_to_string (l: string list) = 
  match l with
  | [] -> ""
  | x::[] -> x 
  | x::xs -> x ^ ", " ^ (list_to_string xs)