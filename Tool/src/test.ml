open Csv
open Array
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


(* Count has to be strictly greater than 0 *)
(* let rec run_test (upTo: int) (count: int) =  *)
(*   if count > upTo  *)
(*   then print_endline("done") *)
(*   else ( *)
(*     (\* let formula = random_formula 200 5 ["a";"b"] in *\) *)
(*     let formula = random_formula count 5 ["a"] in *)
(*       print_endline("formula is " ^ (formula_to_string formula)); *)
(*       procedure formula; *)
(*       run_test upTo (count+2) *)
(*   ) *)

let run_test (min_size:int) (max_size:int) (number_instances:int) =
  let results = make max_size [] in
  for app_size = min_size to max_size do
    for i = 0 to number_instances do
      try (
      let formula = random_formula app_size 0 ["a"] in
      print_endline("Formula is " ^ (formula_to_string formula));
      let t = time (fun () -> get_strongest_mon_cons formula) in
      let size = tree_size formula in
      results.(size) <- t::(results.(size))
      ) with _ -> ()
    done 
  done;
  results

let average = function
  | [] -> 0.
  | l  -> 
    let sum = List.fold_left (+.) 0. l in
    sum /. (float_of_int (List.length l))

let average_a = Array.map average
  

let perform_tests (min_size:int) (max_size:int) (number_instances:int) =
  let timings_tests = run_test min_size max_size number_instances in
  let results = average_a timings_tests in
  Array.iteri (fun i -> fun t -> if i > min_size then Printf.printf "Size: %d Time: %f\n" i t) results

(* This saves an embedded csv in a csv file specified by fname *)
(* Documentation for CSV *)
(* https://github.com/Chris00/ocaml-csv *)

let write_to_file (size: int list) (time: int list) = 
  let test = create_embedded_csv size time in
  let ecsv = Csv.input_all(Csv.of_string test) in
  let fname = "/Users/jasminexuereb/Desktop/phd/OptimalMonitor/Tool/src/example.csv" in
  Csv.save fname ecsv;
  print_endline("Saved CSV to file " ^ fname)
 

(* let rec perform_tests (upTo: int) =  *)
(*   Random.self_init (); *)
(*   run_test upTo 1;   *)
(*   write_to_file [1;2;3] [5;5;5] *)
