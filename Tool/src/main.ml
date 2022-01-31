open Lexing
open PrettyPrint
open StrongestMonCons
open EnvFunctions
open EnvResources 


let parse_formula s = Parser.rechml Lexer.token (from_string s)

let main = 
  let input = (Sys.argv.(1) ^ "\n") in 
    let formula = 
      try parse_formula input 
      with _ ->  
        print_endline("There seems to be some problem parsing your formula!"); 
        exit 0;
    in 
    
    pretty_print_ast formula;
    print_endline("\n");

    let formula = populate_map formula VarSet.empty in
    print_endline("The formula after variable renaming is: " ^ (formula_to_string formula) ^ "\n");

    print_endline("The map is: "); 
    LVars.iter (fun x f -> print_endline( x ^ " -> " ^ (formula_to_string f)) ) !map;
    print_endline("\n");
    
    (* let smc = get_strongest_mon_cons formula in  *)
    (* print_endline("The strongest monitorable consequence is " ^ (formula_to_string smc) ^ "\n"); *)
