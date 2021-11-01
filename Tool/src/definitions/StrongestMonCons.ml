(* The procedure for obtaining the strongest monitorable consequence is three fold:         *)
(* 1. Eliminate the existential modalities by replacing all occurrences of <a>φ by tt       *)
(* 2. Eliminate the least fixed points by replacing all occurrences of min X.φ by max X.φ   *)
(* 3. Remove all disjunctions using the tableau rules                                       *)

open PrettyPrint
open EnvFunctions
open SMCTableauRules
open EnvResources

(* Function that takes a formula and eliminates the existential modalities *)
let rec elim_em (f: Ast.Formula.t): Ast.Formula.t = 
  match f with  
  | Ast.Formula.Verdict(x) -> f 
  | Ast.Formula.LVar(x) -> f 
  | Ast.Formula.Disjunction(x) -> create_disjunction (elim_em x.left) (elim_em x.right)
  | Ast.Formula.Conjunction(x) -> create_conjunction (elim_em x.left) (elim_em x.right)
  | Ast.Formula.Existential(x) -> create_verdict(true)
  | Ast.Formula.Universal(x) -> create_universal x.act (elim_em x.cont)
  | Ast.Formula.Min(x) -> create_min x.lvar (elim_em x.cont)
  | Ast.Formula.Max(x) -> create_max x.lvar (elim_em x.cont)

(* Function that takes a formula and eliminates the minimal fixed points *)
let rec elim_min (f: Ast.Formula.t): Ast.Formula.t =
  match f with 
  | Ast.Formula.Verdict(x) -> f 
  | Ast.Formula.LVar(x) -> f 
  | Ast.Formula.Disjunction(x) -> create_disjunction (elim_em x.left) (elim_em x.right)
  | Ast.Formula.Conjunction(x) -> create_conjunction (elim_em x.left) (elim_em x.right)
  | Ast.Formula.Existential(x) -> create_existential x.act (elim_em x.cont)
  | Ast.Formula.Universal(x) -> create_universal x.act (elim_em x.cont)
  | Ast.Formula.Min(x) -> create_max x.lvar (elim_em x.cont)
  | Ast.Formula.Max(x) -> create_max x.lvar (elim_em x.cont)

let get_strongest_mon_cons (f: Ast.Formula.t): Ast.Formula.t = 
  
  let step1 = elim_em f in 
    print_string("After eliminating existential modalities, we get ");
    pretty_print_formula step1; 
    print_endline("\n");
    
    let step1_smp = apply_laws step1 in
      print_string("After simplifying, we get ");
      pretty_print_formula step1_smp;
      print_endline("\n");

      let step2 = elim_min step1_smp in
        print_string("After eliminating minimal fixed points, we get ");
        pretty_print_formula step2;
        print_endline("\n");
        
        let tableau = formula_to_tableau (FormulaSet.singleton step2) in
          print_endline("The Tableau for Eliminating the Disjunctions is: \n");
          print_endline (print_tab_tree tableau "");
                
          let relabelled = relabel_tableau tableau in
            print_endline("\nThe Tableau after relabelling is: \n");
            print_endline (print_relabelled_tab relabelled "");

        step2

