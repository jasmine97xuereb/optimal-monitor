
(* The procedure for obtaining the strongest monitorable consequence is three fold:         *)
(* 1. Eliminate the existential modalities by replacing all occurrences of <a>φ by tt       *)
(* 2. Eliminate the least fixed points by replacing all occurrences of min X.φ by max X.φ   *)
(* 3. Remove all disjunctions using the tableau rules                                       *)

open PrettyPrint
open EnvFunctions
open SMCTableauRules
open EnvResources

(* Function that takes a formula and eliminates the existential modalities *)
let rec elim_em (f: Ast.formula): Ast.formula = 
  match f with  
  | TT | FF | LVar _ -> f 
  | Disjunction(l, r) -> Disjunction((elim_em l), (elim_em r))
  | Conjunction(l, r) -> Conjunction((elim_em l), (elim_em r))
  | Existential _ -> TT
  | Universal(a, cont) -> Universal(a, (elim_em cont))
  | Min(x, cont) -> Min(x, (elim_em cont))
  | Max(x, cont) -> Max(x, (elim_em cont))

(* Function that takes a formula and eliminates the minimal fixed points *)
let rec elim_min (f: Ast.formula): Ast.formula =
  match f with 
  | TT | FF | LVar _ -> f
  | Disjunction(l, r) -> Disjunction(elim_min l, elim_min r)
  | Conjunction(l, r) -> Conjunction(elim_min l, elim_min r)
  | Existential(a, cont) -> Existential(a, elim_min cont)
  | Universal(a, cont) -> Universal(a, elim_min cont)
  | Min(x, cont) -> Max(x, elim_min cont)
  | Max(x, cont) -> Max(x, elim_min cont)

let get_strongest_mon_cons (f: Ast.formula): Ast.formula = 
  
  let step1 = elim_em f in 
    print_endline("After eliminating existential modalities, we get " ^ (formula_to_string step1) ^ "\n"); 
    
    (* let step1_smp = simplify step1 in *)
    let step1_smp = step1 in
      print_endline("After simplifying, we get " ^ (formula_to_string step1_smp) ^ "\n");

      let step2 = elim_min step1_smp in
        print_endline("After eliminating minimal fixed points, we get " ^ (formula_to_string step2) ^ "\n");

        update_map step2; 
        (* print_endline("The updated map is: ");
        LVars.iter (fun x f -> print_endline( x ^ " -> " ^ (formula_to_string f)) ) !map;
        print_endline("\n"); *)
        
        if disjunction_free step2 
        then step2
        else ( 
          let tableau = formula_to_tableau (FormulaSet.singleton step2) in
            print_endline("The Tableau for Eliminating the Disjunctions is: \n" ^ (print_tab_tree tableau ""));
                  
            let relabelled = relabel_tableau tableau in
              print_endline("\nThe Tableau after relabelling is: \n" ^ (print_relabelled_tab relabelled ""));

              let smc = 
                match relabelled with
                | Leaf(x) -> x.leaf_value
                | Node(x) -> x.node_value
              in 
              
              let smp_smc = simplify smc in
              print_endline("\nAfter simplifying, we get " ^ (formula_to_string smp_smc) ^ "\n");
              smp_smc
        )