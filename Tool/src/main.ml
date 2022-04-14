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

let check_test (input): unit = 
  if (Array.length input == 2) && (input.(1) = "test")
  then (
    print_endline("Please, be patient... This might take a lot of time...");
    let file_dir = (Sys.getcwd()) ^ "/../Evaluation/results_random.csv"
    in perform_tests file_dir 1 15000 500; 
    process_tests file_dir;
    exit 0
  )
  else () 
  
let check_save (input): bool = 
  if (Array.length input == 2) && (input.(1) = "save")
  then true
  else 
    if (Array.length input == 3) && (input.(2) = "save")
    then true
    else false

let get_path () = 
  print_endline("\nPlease enter the full file path.");
  let file_path = Scanf.scanf "%s" (fun x -> x) 
  in let content = read_lines file_path
  in print_endline("\nThe formula read is: \n" ^ content);
  content ^ "\n"

let read_formula (input): string = 
  if Array.length input == 1
  then (print_endline("len is 1"); get_path ())
  else if Array.length input == 2
  then (
    if input.(1) = "save"
    then (print_endline("len is 2 but save");  get_path ())
    else (input.(1) ^ "\n")
  ) 
  else 
    if Array.length input == 3
    then (input.(1) ^ "\n") 
    else ""




let main =  

  let input = Sys.argv in 
  check_test input;
  let save = check_save input in
  let read = read_formula input in
  let formula =
    try parse_formula read
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
  let smc = procedure formula 
    in print_endline("The strongest monitorable consequence is " ^ (formula_to_string smc) ^ "\n");  
    if save 
    then generate_property_detecter smc
    else () 
  