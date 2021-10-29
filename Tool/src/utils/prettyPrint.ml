(* Print the recHML formula *)

let rec formula_to_string (input: Ast.Formula.t): string = 
  match input with
  | Ast.Formula.Verdict(x) -> if x.verdict then "tt" else "ff"  
  | Ast.Formula.LVar(x) -> x.lvar 
  | Ast.Formula.Disjunction(x) -> (formula_to_string x.left) ^ " | " ^ (formula_to_string x.right)
  | Ast.Formula.Conjunction(x) -> (formula_to_string x.left) ^ " & " ^ (formula_to_string x.right)
  | Ast.Formula.Existential(x) -> "<" ^ (x.act.name) ^ ">" ^ (formula_to_string x.cont)
  | Ast.Formula.Universal(x) -> "[" ^ (x.act.name) ^ "]" ^ (formula_to_string x.cont) 
  | Ast.Formula.Min(x) -> "min " ^ x.lvar.lvar ^ "." ^ (formula_to_string x.cont) 
  | Ast.Formula.Max(x) -> "max " ^ x.lvar.lvar ^ "." ^ (formula_to_string x.cont) 

let rec pretty_print_formula (formula: Ast.Formula.t): unit = 
  print_string((formula_to_string formula) ^ "; ")
  
let rec tabulate tab = match tab with
  | 0 -> ""
  | _ -> "  " ^ tabulate (tab - 1)

let rec pretty_print_ast (input: Ast.Formula.t) (tab) = 
  let f_to_s tree tab = 
    match tree with
    | Ast.Formula.Verdict(x) -> print_verdict x tab
    | Ast.Formula.LVar(x) -> print_lvar x tab
    | Ast.Formula.Disjunction(x) -> print_disjunction x tab
    | Ast.Formula.Conjunction(x) -> print_conjunction x tab
    | Ast.Formula.Existential(x) -> print_existential x tab
    | Ast.Formula.Universal(x) -> print_universal x tab
    | Ast.Formula.Min(x) -> print_min x tab
    | Ast.Formula.Max(x) -> print_max x tab
  in tabulate tab ^ "<Formula>\n" ^
    f_to_s input (tab + 1) ^
    tabulate tab ^ "</Formula>"

  and print_action (act: string) (tab: int): string = 
    tabulate tab ^ "<Act>\n" ^
    tabulate (tab + 1) ^ act ^ "\n" ^
    tabulate tab ^ "</Act>\n"

  and print_lvar (lvar: Ast.Formula.LVar.t) tab =
    tabulate tab ^ "<LVar>\n" ^
    tabulate (tab + 1) ^ lvar.lvar ^ "\n" ^
    tabulate tab ^ "</LVar>\n"

  and print_verdict (v: Ast.Formula.Verdict.t) (tab: int): string  = 
    match v.verdict with
    | true -> tabulate tab ^ "<Verdict>\n" ^ tabulate (tab + 1) ^ "tt \n" ^ tabulate tab ^ "</Verdict>\n"
    | false -> tabulate tab ^ "<Verdict>\n" ^ tabulate (tab + 1) ^ "ff \n" ^ tabulate tab ^ "</Verdict>\n"

  and print_disjunction (tree: Ast.Formula.Disjunction.t) (tab: int): string = 
    tabulate tab ^ "<Disjunction>\n" ^
    (pretty_print_ast tree.left (tab + 1)) ^ "\n" ^ (pretty_print_ast tree.right (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Disjunction>\n"

  and print_conjunction (tree: Ast.Formula.Conjunction.t) (tab: int): string = 
    tabulate tab ^ "<Conjunction>\n" ^
    (pretty_print_ast tree.left (tab + 1)) ^ "\n" ^ (pretty_print_ast tree.right (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Conjunction>\n"

  and print_existential (tree: Ast.Formula.Existential.t) (tab: int): string = 
    tabulate tab ^ "<Existential>\n" ^
    (print_action tree.act.name (tab + 1)) ^ (pretty_print_ast tree.cont (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Existential>\n"

  and print_universal (tree: Ast.Formula.Universal.t) (tab: int): string = 
    tabulate tab ^ "<Universal>\n" ^
    (print_action tree.act.name (tab + 1)) ^ (pretty_print_ast tree.cont (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Universal>\n"
    
  and print_min (tree: Ast.Formula.Min.t) (tab: int): string =
    tabulate tab ^ "<Min>\n" ^
    print_lvar tree.lvar (tab + 1) ^
    pretty_print_ast tree.cont (tab + 1) ^ "\n" ^
    tabulate tab ^ "</Min>\n"

  and print_max (tree: Ast.Formula.Max.t) (tab: int): string =
    tabulate tab ^ "<Max>\n" ^
    print_lvar tree.lvar (tab + 1) ^ "\n" ^
    pretty_print_ast tree.cont (tab + 1) ^ "\n" ^
    tabulate tab ^ "</Max>\n"

    

  