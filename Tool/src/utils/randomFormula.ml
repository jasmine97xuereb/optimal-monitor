open Ast
open EnvFunctions

exception Random_formula_exception
exception Empty_List
exception Empty_Interval
exception Random_int_exception
exception Null
exception Insert_out_of_bounds

let variable index = string_of_int(index)

(* Returns a list of n copies of the input element *)
let rec duplicate elt = function
  | 0 -> []
  | n -> elt::(duplicate elt (n-1))

(* Returns a list of n random integers in [1;max] *)
let rec random_int_list (max:int) (n:int) =
  if max = 0 then raise Empty_Interval
  else if max = 1 then duplicate 1 n
  else
    let rec random_int_list_h = function
      | 0 -> []
      | n -> ((Random.int max) + 1)::(random_int_list_h (n-1))
    in
    random_int_list_h n

(* Return the sum of the elements of a list *)
let sum_list = List.fold_left (+) 0

(* Insert el at position i in lst *)
let rec insert i el lst =
  if i = 0 then el::lst
  else match lst with
    | [] -> raise Insert_out_of_bounds
    | h::t -> h::(insert (i-1) el t)

(* Randomly inserts an element in a list *)
let random_insert_elt el = function
  | [] -> [el]
  | lst  -> let i = Random.int (List.length lst) in insert i el lst

(* Returns a list of n random integers whose sum is s *)
let random_int_sum (s:int) (n:int) =
  let random_ints = random_int_list s n in
  let d = sum_list random_ints in
  let l = List.filter (fun n -> n > 0) (List.rev_map (fun k -> (k * s) / d) random_ints) in
  let diff = s - sum_list l in
  if diff = 0 then l
  else random_insert_elt diff l

(* Debugging function: checks if one of the elements is zero *)
let check_zero = List.exists (fun n -> n = 0)

(* Picks a random action in actions *)
let random_action (actions:action list) =
  let a = List.length actions in
  if a = 0 then raise Empty_List
  else
    let n = Random.int a in
    List.nth actions n

(* Randomly assigns an action to each formula in the list *)
let random_assign_actions (actions:action list) =
  List.rev_map (fun phi -> (random_action actions, phi))

(* Randomly picks a subset of actions *)
let rec random_action_set = function
  | [] -> []
  | h::t -> match Random.bool() with
    | true -> h::(random_action_set t)
    | false -> random_action_set t

(* Randomly picks a nonempty subset of actions *)
let rec random_nonempty_action_set (actions: action list) =
  let r = random_action_set actions in
    if List.length r > 0 then r
    else random_nonempty_action_set actions

(* Returns the conjuction of <act> phi for all phi in the list *)
let rec conj_act (act:action) = function
  | []     -> TT
  | [phi]  -> Existential(act,phi)
  | phi::t -> Conjunction(Existential(act,phi),conj_act act t)

(* Returns the disjunction of all formulas *)
let rec disj = function
  | []     -> FF
  | [phi]  -> phi
  | phi::t -> Disjunction(phi,disj t)

(* Returns --a--> formulas *)
let arrow_act (act:action) formulas =
  Conjunction(conj_act act formulas, Universal(act,disj formulas))

(* Returns --a--> formulas for a list of formulas associated with act.
 * Assumes that all actions are act *)
let arrow_act_ass (act:action) (act_forms:(action*Ast.formula) list) =
  arrow_act act (List.map (fun (a,phi) -> phi) act_forms)

(* Returns --A--> {Ba | a € actions} *)
let rec arrow (actions:action list) (act_forms:(action*Ast.formula) list) =
  match actions with
    | []     -> TT
    | [act]  -> arrow_act_ass act (List.filter (fun (x,phi) -> x = act) act_forms)
    | act::t -> let (la,na) = List.partition (fun (x,phi) -> x = act) act_forms in
      Conjunction(arrow_act_ass act la, arrow t na)

(* Randomly assigns formulas to actions to get a partion into Ba (a € actions) *)
let random_arrow_from_formulas (actions:action list) (formulas:Ast.formula list) =
  let random_act_forms = random_assign_actions actions formulas in
    arrow actions random_act_forms

(* Returns a random formula in DNF
 * of size size
 * with variables ranging in [X0,...,X_(max_var-1)]
 * and actions in actions *)
let rec random_formula (size:int) (max_var:int) (actions:action list): Ast.formula =
  let a = List.length actions in
  if size = 0 then raise Random_formula_exception
  else if size = 1 then
    if max_var = 0 then
      match Random.bool() with
        | true  -> TT
        | false -> FF
    else
      match Random.int 3 with
        | 0 -> TT
        | 1 -> FF
        | 2 -> let index = Random.int max_var in LVar("X" ^ variable(index))
        | _ -> raise Random_int_exception
  else if size = 2 then
    match Random.bool() with
      | true  -> Min("X" ^ variable(max_var+1), random_formula (size-1) (max_var+1) actions)
      | false -> Max("X" ^ variable(max_var+1), random_formula (size-1) (max_var+1) actions)
  else if size - a < 5 then
    match Random.int 3 with
      | 0 -> let size_left = Random.int (size - 2) + 1 in Disjunction(random_formula size_left max_var actions, random_formula (size - size_left - 1) max_var actions)
      | 1 -> Min("X" ^ variable(max_var+1), random_formula (size-1) (max_var+1) actions)
      | 2 -> Max("X" ^ variable(max_var+1), random_formula (size-1) (max_var+1) actions)
      | _ -> raise Random_int_exception
  else
    match Random.int 4 with
      | 0 -> let size_left = Random.int (size - 2) + 1 in Disjunction(random_formula size_left max_var actions, random_formula (size - size_left - 1) max_var actions)
      | 1 -> Min("X" ^ variable(max_var+1), random_formula (size-1) (max_var+1) actions)
      | 2 -> Max("X" ^ variable(max_var+1), random_formula (size-1) (max_var+1) actions)
      | 3 ->
        let n = (Random.int ((size - a)/5)) + 1 in
        let l = random_int_sum ((size - a - 3*n)/2) n in
        let random_formulas = List.rev_map (fun n -> random_formula n max_var actions) l in
          (* List.iter (fun phi -> Printf.printf("%d; ") (tree_size phi)) random_formulas; *)
          (* let form = random_arrow_from_formulas actions random_formulas in *)
          (*  Printf.printf "\ns=%d\nn=%d\nl=%d\nThe required size is %d, I got %d\n" n n (sum_list l) size (tree_size form); *)
          (* form; *)
         random_arrow_from_formulas actions random_formulas
      | _ -> raise Random_int_exception

  
