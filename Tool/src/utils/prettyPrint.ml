let rec formula_to_string (input: Ast.formula): string = 
  match input with
  | TT -> "tt"
  | FF -> "ff"  
  | LVar(x) -> x
  | Disjunction(l, r) -> (formula_to_string l) ^ " | " ^ (formula_to_string r)
  | Conjunction(l, r) -> (formula_to_string l) ^ " & " ^ (formula_to_string r)
  | Existential(a, cont) -> "<" ^ a ^ ">" ^ (formula_to_string cont)
  | Universal(a, cont) -> "[" ^ a ^ "]" ^ (formula_to_string cont) 
  | Min(x, cont) -> "min " ^ x ^ "." ^ (formula_to_string cont) 
  | Max(x, cont) -> "max " ^ x ^ "." ^ (formula_to_string cont) 

let rec pretty_print_formula (formula: Ast.formula): unit = 
  print_string((formula_to_string formula) ^ "; ")
  
let rec tabulate tab = match tab with
  | 0 -> ""
  | _ -> "  " ^ tabulate (tab - 1)

let rec pretty_print_ast (input: Ast.formula) (tab) = 
  let f_to_s (tree: Ast.formula) tab = 
    match tree with
    | TT -> print_tt tab
    | FF -> print_ff tab
    | LVar(x) -> print_lvar x tab
    | Disjunction(l, r) -> print_disjunction l r tab
    | Conjunction(l, r) -> print_conjunction l r tab
    | Existential(a, cont) -> print_existential a cont tab
    | Universal(a, cont) -> print_universal a cont tab
    | Min(x, cont) -> print_min x cont tab
    | Max(x, cont) -> print_max x cont tab
  in tabulate tab ^ "<Formula>\n" ^
    f_to_s input (tab + 1) ^
    tabulate tab ^ "</Formula>"

  and print_action (act: Ast.action) (tab: int): string = 
    tabulate tab ^ "<Act>\n" ^
    tabulate (tab + 1) ^ act ^ "\n" ^
    tabulate tab ^ "</Act>\n"

  and print_lvar (x: Ast.variable) tab =
    tabulate tab ^ "<LVar>\n" ^
    tabulate (tab + 1) ^ x ^ "\n" ^
    tabulate tab ^ "</LVar>\n"

  and print_tt (tab: int): string  = 
    tabulate tab ^ "<Verdict>\n" ^ tabulate (tab + 1) ^ "tt \n" ^ tabulate tab ^ "</Verdict>\n"

  and print_ff (tab: int): string  = 
    tabulate tab ^ "<Verdict>\n" ^ tabulate (tab + 1) ^ "ff \n" ^ tabulate tab ^ "</Verdict>\n"

  and print_disjunction (l: Ast.formula) (r: Ast.formula) (tab: int): string = 
    tabulate tab ^ "<Disjunction>\n" ^
    (pretty_print_ast l (tab + 1)) ^ "\n" ^ (pretty_print_ast r (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Disjunction>\n"

  and print_conjunction (l: Ast.formula) (r: Ast.formula) (tab: int): string = 
    tabulate tab ^ "<Conjunction>\n" ^
    (pretty_print_ast l (tab + 1)) ^ "\n" ^ (pretty_print_ast r (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Conjunction>\n"

  and print_existential (a: Ast.action) (cont: Ast.formula) (tab: int): string = 
    tabulate tab ^ "<Existential>\n" ^
    (print_action a (tab + 1)) ^ (pretty_print_ast cont (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Existential>\n"

  and print_universal (a: Ast.action) (cont: Ast.formula) (tab: int): string = 
    tabulate tab ^ "<Universal>\n" ^
    (print_action a (tab + 1)) ^ (pretty_print_ast cont (tab + 1)) ^ " \n " ^
    tabulate tab ^ "</Universal>\n"
    
  and print_min (x: Ast.variable) (cont: Ast.formula) (tab: int): string =
    tabulate tab ^ "<Min>\n" ^
    print_lvar x (tab + 1) ^
    pretty_print_ast cont (tab + 1) ^ "\n" ^
    tabulate tab ^ "</Min>\n"

  and print_max (x: Ast.variable) (cont: Ast.formula) (tab: int): string =
    tabulate tab ^ "<Max>\n" ^
    print_lvar x (tab + 1) ^ "\n" ^
    pretty_print_ast cont (tab + 1) ^ "\n" ^
    tabulate tab ^ "</Max>\n"