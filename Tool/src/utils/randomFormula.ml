open Ast

let variable index = string_of_int(index)

let rec random_int_list (max:int) = function
  | 0 -> []
  | n -> (Random.int (max-1) +1)::(random_int_list max (n-1))

let random_int_sum (sum:int) (size:int) =
  let l = random_int_list sum size in
  let s = List.fold_left (+) 0 l in
  List.rev_map (fun n -> (n * sum) / s) l

let random_action (actions:action list) =
  let n = Random.int (List.length actions) in
    List.nth actions n

let random_assign (actions:action list) (formulas:Ast.formula list) =
  List.map (fun phi -> (random_action actions, phi)) formulas

let rec random_action_set = function
  | [] -> []
  | h::t -> match Random.bool () with
    | true -> h::(random_action_set t)
    | false -> random_action_set t

let rec random_nonempty_action_set (actions: action list) =
  let r = random_action_set actions in
    if List.length r > 0 then r
    else random_nonempty_action_set actions

let random_gen_actions (actions:action list) =
  List.rev_map (fun phi -> (random_action actions, phi))

let rec conj_act (act:action) = function
  | []     -> TT
  | [phi]  -> Existential(act,phi)
  | phi::t -> Conjunction(Existential(act,phi),conj_act act t)

let rec disj = function
  | []     -> FF
  | [phi]  -> phi
  | phi::t -> Disjunction(phi,disj t)

let arrow_act (act:action) formulas =
  Conjunction(conj_act act formulas, Universal(act,disj formulas))

(* Assumes that all actions are act *)
let arrow_act_ass (act:action) (act_forms:(action*Ast.formula) list) =
  arrow_act act (List.map (fun (a,phi) -> phi) act_forms)

let rec arrow (actions:action list) (act_forms:(action*Ast.formula) list) =
  match actions with
    | []     -> TT
    | [act]  -> arrow_act_ass act (List.filter (fun (x,phi) -> x = act) act_forms)
    | act::t -> let (la,na) = List.partition (fun (x,phi) -> x = act) act_forms in
      Conjunction(arrow_act_ass act la, arrow t na)

let random_arrow_from_formulas (actions:action list) (formulas:Ast.formula list) =
  let random_act_forms = random_gen_actions actions formulas in
  arrow actions random_act_forms

let rec random_formula (size:int) (max_var:int) (actions:action list): Ast.formula =
  if size = 1 then
    let coin = Random.int 3 in
      match coin with
        | 0 -> TT
        | 1 -> FF
        | _ -> let index = Random.int max_var in LVar(variable(index))
  else
    let coin = Random.int 4 in
      match coin with
        | 0 -> let size_left = Random.int (size - 3) + 1 in Disjunction(random_formula size_left max_var actions, random_formula (size - size_left) max_var actions)
        | 1 -> Min(variable(max_var+1), random_formula (size-1) (max_var+1) actions)
        | 2 -> Max(variable(max_var+1), random_formula (size-1) (max_var+1) actions)
        | _ ->
          let a = random_nonempty_action_set actions in
          let s = List.length a in
          let n = Random.int (size - s) + 1 in
          let l = random_int_sum (size - s) n in
          let random_formulas = List.rev_map (fun n -> random_formula n max_var actions) l in
            random_arrow_from_formulas a random_formulas
