module B = PrintBox

let rec formula_to_box (f: Ast.formula) = 
  match f with 
  | TT -> B.tree (B.text "tt") []
  | FF -> B.tree (B.text "ff") []
  | Disjunction(f1,f2) -> B.tree (B.text "OR") [formula_to_box f1;formula_to_box f2]
  | Conjunction(f1,f2) -> B.tree (B.text "AND") [formula_to_box f1;formula_to_box f2]
  | Existential(a,f) -> B.tree (B.text ("<"^a^">")) [formula_to_box f]
  | Universal(a,f) -> B.tree (B.text ("["^a^"]")) [formula_to_box f]
  | Min(x,f) -> B.tree (B.text ("Min "^x)) [formula_to_box f]
  | Max(x,f) -> B.tree (B.text ("Max "^x)) [formula_to_box f]
  | LVar(x) -> B.tree (B.text x) []
and pretty_print_ast (f: Ast.formula) = PrintBox_text.output stdout (formula_to_box f)

let rec formula_to_string (input: Ast.formula): string = 
  match input with
  | TT -> "tt"
  | FF -> "ff"  
  | LVar(x) -> x
  | Disjunction(l, r) -> (formula_to_string l) ^ " | " ^ (formula_to_string r)
  | Conjunction(l, r) -> (formula_to_string l) ^ " & " ^ (formula_to_string r)
  | Existential(a, cont) -> (match cont with
                              | Disjunction _ -> "<" ^ a ^ ">(" ^ (formula_to_string cont) ^ ")"
                              | Conjunction _ -> "<" ^ a ^ ">(" ^ (formula_to_string cont) ^ ")"
                              | _ -> "<" ^ a ^ ">" ^ (formula_to_string cont))
  | Universal(a, cont) -> (match cont with
                              | Disjunction _ -> "[" ^ a ^ "](" ^ (formula_to_string cont) ^ ")"
                              | Conjunction _ -> "[" ^ a ^ "](" ^ (formula_to_string cont) ^ ")"
                              | _ -> "[" ^ a ^ "]" ^ (formula_to_string cont))                            
  | Min(x, cont) -> "min " ^ x ^ "." ^ (formula_to_string cont) 
  | Max(x, cont) -> "max " ^ x ^ "." ^ (formula_to_string cont) 

