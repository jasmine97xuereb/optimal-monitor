# #require "printbox";;
# #require "printbox-text";;

# module B = PrintBox;;
module B = PrintBox
(* Print the recHML formula *)

let formula_to_box = fun
                   | TT -> B.tree (B.text "tt") []
                   | FF -> B.tree (B.text "ff") []
                   | Disjunction(f1,f2) -> B.tree (B.text "OR") [formula_to_box f1;formula_to_box f2]
                   | Conjunction(f1,f2) -> B.tree (B.text "AND") [formula_to_box f1;formula_to_box f2]
                   | Existential(a,f) -> B.tree (B.text "<"+a+">") [formula_to_box f]
                   | Universal(a,f) -> B.tree (B.text "["+a+"]") [formula_to_box f]
                   | Min(x,f) -> B.tree (B.text "Min "+ x) [formula_to_box f]
                   | Max(x,f) -> B.tree (B.text "Max "+ x) [formula_to_box f]
                   | Var(x) -> B.tree (B.text x)


let pretty_print_ast f  = PrintBox_text.output stdout (formula_to_box f)
