(* open Array
open RandomFormula
open PrettyPrint
open EnvFunctions
open EnvResources
open StrongestMonCons

(* Counts the time that elapses during the computation of f x *)
let time f =
    let start = Unix.gettimeofday ()
    in let res = f ()
    in let stop = Unix.gettimeofday ()
    (* in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start) *)
    in
       (stop -. start)

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


let perform_tests (min_size:int) (max_size:int) (number_instances:int) =
  print_endline("here");
  let oc = 
    try open_out "/Users/jasminexuereb/Desktop/phd/OptimalMonitor/Tool/src/results.csv" 
    with _ -> print_endline("couldn't open file");
    exit 0
    in
  for app_size = min_size to max_size do
    for i = 0 to number_instances do
      try (
      let formula = random_formula app_size 0 ["a"] in
      let t = time (fun () -> get_strongest_mon_cons formula) in
      let size = tree_size formula in
      Printf.fprintf oc "%d,%f\n" size t;
      ) with _ -> ()
    done 
  done;
  close_out oc;;

let average = function
  | [] -> 0.
  | l  -> 
    let sum = List.fold_left (+.) 0. l in
    sum /. (float_of_int (List.length l))

let average_a = Array.map average

let process_tests =
  let csvresults = Csv.load "/Users/jasminexuereb/Desktop/phd/OptimalMonitor/Tool/src/results.csv" in
  let strresults = Csv.to_array csvresults in
  let cresults = Array.map (fun line -> (int_of_string line.(0),float_of_string line.(1))) strresults in
  let max_instance = Array.fold_left (fun m -> fun (size,_) -> max m size) 0 cresults in
  let lresults = Array.make (max_instance + 1) []  in
  Array.iter (fun (size,time) -> lresults.(size) <- time::(lresults.(size))) cresults;
  let results = average_a lresults in
  let strlresults = Array.mapi (fun size ->  fun time -> Array.of_list [string_of_int size;string_of_float time]) results in
  let csvtresults = Csv.of_array strlresults in
  Csv.save "/Users/jasminexuereb/Desktop/phd/OptimalMonitor/Tool/src/processed_results.csv" csvtresults;
   *)
