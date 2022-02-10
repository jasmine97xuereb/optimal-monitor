open Lexing
open PrettyPrint
open StrongestMonCons
open EnvFunctions
open EnvResources 
open Test

(* let parse_formula s = Parser.rechml Lexer.token (from_string s)

let main = 
  Random.self_init ();
  (* let formula = random_formula 200 5 ["a";"b"] in *)
  let input = (Sys.argv.(1) ^ "\n") in
    let formula =
      try parse_formula input
      with _ ->
        print_endline("There seems to be some problem parsing your formula!");
        exit 0;
    in
    
    let size = tree_size formula in 
    print_endline("tree size is " ^ string_of_int(size));

    pretty_print_ast formula;
    print_endline("\n");

    let formula = populate_map formula VarSet.empty in
    (* print_endline("The formula after variable renaming is: " ^ (formula_to_string formula) ^ "\n"); *)

    (* print_endline("The map is: "); 
    LVars.iter (fun x f -> print_endline( x ^ " -> " ^ (formula_to_string f)) ) !map;
    print_endline("\n");
     *)
    let smc = get_strongest_mon_cons formula in 
    print_endline("The strongest monitorable consequence is " ^ (formula_to_string smc) ^ "\n");
 *)



let parse_formula s = Parser.rechml Lexer.token (from_string s)

let procedure (formula: Ast.formula): Ast.formula = 
  let size = tree_size formula in 
    print_endline("tree size is " ^ string_of_int(size));
    pretty_print_ast formula;
    print_endline("\n");

    let formula = populate_map formula VarSet.empty in
      let smc = get_strongest_mon_cons formula in 
        print_endline("The strongest monitorable consequence is " ^ (formula_to_string smc) ^ "\n");
        smc

let main = 
  (* let formula = random_formula 200 5 ["a";"b"] in *)
  let input = (Sys.argv.(1) ^ "\n") in
    let formula =
      try parse_formula input
      with _ ->
        print_endline("There seems to be some problem parsing your formula!");
        exit 0 
    in
    
    let smc = procedure formula  
      in print_endline("The strongest monitorable consequence is " ^ (formula_to_string smc) ^ "\n");

      perform_tests 2 

    

 









    (* let rand = random_formula 5 0 (["a"])
    in print_endline("Generated rand is " ^ (formula_to_string rand) ^ "\n");
    pretty_print_ast rand; *)


