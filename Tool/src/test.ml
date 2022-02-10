open Csv
open RandomFormula
open PrettyPrint
open EnvFunctions
open EnvResources
open StrongestMonCons


let procedure (formula: Ast.formula) = 
  let size = tree_size formula in 
    print_endline("tree size is " ^ string_of_int(size));
    pretty_print_ast formula;
    print_endline("\n");

    let formula = populate_map formula VarSet.empty in
      let smc = get_strongest_mon_cons formula in 
        print_endline("The strongest monitorable consequence is " ^ (formula_to_string smc) ^ "\n")


(* This function takes two lists and returns an embedded csv, which is a string *)

let rec create_embedded_csv (size: int list) (time: int list) =   
  match size, time with
  | h1::tl1, h2::tl2 -> "\"" ^ string_of_int(h1) ^ "\",\"" ^ string_of_int(h2) ^ "\"\n" ^ create_embedded_csv tl1 tl2 
  | _, _ -> ""


(* Count has to be strictly greater than 0 *)
let rec run_test (upTo: int) (count: int) = 
  if count > upTo 
  then print_endline("done")
  else (
    let formula = random_formula count 5 ["a"] in
      print_endline("formula is " ^ (formula_to_string formula));
      procedure formula;
      run_test upTo (count+2)
  )

(* This saves an embedded csv in a csv file specified by fname *)
(* Documentation for CSV *)
(* https://github.com/Chris00/ocaml-csv *)

let write_to_file (size: int list) (time: int list) = 
  let test = create_embedded_csv size time in
  let ecsv = Csv.input_all(Csv.of_string test) in
  let fname = "/Users/jasminexuereb/Desktop/phd/OptimalMonitor/Tool/src/example.csv" in
  Csv.save fname ecsv;
  print_endline("Saved CSV to file " ^ fname)
 

let rec perform_tests (upTo: int) = 
  Random.self_init ();
  run_test upTo 1;  
  write_to_file [1;2;3] [5;5;5]