open Lexing
open PrettyPrint
open StrongestMonCons
open EnvFunctions
open EnvResources 
open Test

(* Read the contents of a file and return a string *)
let read_lines name : string =
  let ic = 
    try open_in name 
    with _ -> (
      print_endline("\nCannot open file. Aborting.\n");
      exit 0 
    )  
  in let try_read () =
    try Some (input_line ic) with End_of_file -> None 
  in let rec loop acc = match try_read () with
    | Some s -> loop (acc ^ s)
    | None -> (close_in ic); acc  
  in loop ""

let parse_formula s = Parser.rechml Lexer.token (from_string s)

let procedure (formula: Ast.formula): Ast.formula = 
  let formula = populate_map formula VarSet.empty in
    let smc = get_strongest_mon_cons formula in 
      smc

let main =  
  (* perform_tests 1 15000 500  *)

  let input = 
    if Array.length Sys.argv > 1
    then (
      (Sys.argv.(1) ^ "\n")
    ) 
    else (
      print_endline("\nPlease enter the full file path.");
      let file_path = Scanf.scanf "%s" (fun x -> x) 
      in let content = read_lines file_path
      in print_endline("\nThe formula read is: \n" ^ content);
      content ^ "\n"
    )
    in
    let formula =
      try parse_formula input
      with _ ->
        print_endline("There seems to be some problem parsing your formula!");
        exit 0 
    
    in 
    print_endline("\nThe AST is: ");
    pretty_print_ast formula;
    print_endline("\n");
  
    if not (VarSet.is_empty (fv formula VarSet.empty))
    then (
      print_endline("The formula is not closed. Aborting.");
      exit 0
    )
    else
    let size = tree_size formula in 
    (* print_endline("tree size is " ^ string_of_int(size)); *)
    let smc = procedure formula 
      in print_endline("The strongest monitorable consequence is " ^ (formula_to_string smc) ^ "\n");  
      smc
      (* generate_property_detecter smc *)

    





