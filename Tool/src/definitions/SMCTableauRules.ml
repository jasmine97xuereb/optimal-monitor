open EnvFunctions
open EnvResources
open PrettyPrint
open Ast

(* For each node n and its children, there is a rule st n is labelled with the premise and the children with its conclusion                 *)
(* The tableu rule is the rule [a] only if no other rule can be applied                                                                     *)
(* The function apply_rules takes a formula list, applies a rule, and returns the conclusion and the rule applied                           *)
(* Note that this function returns a list of Formula sets because the conclusion of rule [a] is two sets, each representing a tree branch   *)

let rec apply_rules (unseen: Ast.Formula.t list) (seen: Ast.Formula.t list): FormulaSet.t list * rule = 
  match unseen with 
  | [] -> ([], None)
  | f::fs -> (
    match f with 
    | Ast.Formula.Verdict(x) -> apply_verdict x (fs @ seen)
    | Ast.Formula.Disjunction(x) -> ([apply_disjunction x (fs @ seen)], Disjunction)
    | Ast.Formula.Conjunction(x) -> (apply_conjunction x (fs @ seen), Conjunction)
    | Ast.Formula.Max(x) -> ([apply_max x (fs @ seen)], Max)
    | Ast.Formula.LVar(x) -> ( [ FormulaSet.of_list ( [LVars.find x !map] @ fs @ seen) ], X)
    | Ast.Formula.Universal(x) -> apply_universal x fs ([f] @ seen)
    | _ -> ([], None)
  ) 

and apply_verdict (f: Ast.Formula.Verdict.t) (rem: Ast.Formula.t list): FormulaSet.t list * rule = 
  if f.verdict 
  then ([ FormulaSet.singleton (create_verdict f.verdict) ], TT) 
  else ([ FormulaSet.of_list rem ], FF)

and apply_disjunction (f: Ast.Formula.Disjunction.t) (rem: Ast.Formula.t list): FormulaSet.t =
  FormulaSet.of_list ([f.left] @ [f.right] @ rem)  

and apply_conjunction (f: Ast.Formula.Conjunction.t) (rem: Ast.Formula.t list): FormulaSet.t list =
  [ FormulaSet.of_list ([f.left] @ rem) ] @ [ FormulaSet.of_list ([f.right] @ rem) ]

and apply_max (f: Ast.Formula.Max.t) (rem: Ast.Formula.t list): FormulaSet.t =
  FormulaSet.of_list (rem @ [f.cont]) 

(* Apply rule [a] *)
and apply_universal_a (f: Ast.Formula.Universal.t) (rem: Ast.Formula.t list): FormulaSet.t = 
  let filter = fun x -> 
    match x with
    | Ast.Formula.Universal(y) -> if y.act = f.act then Some y.cont else None 
    | _ -> None
  in FormulaSet.of_list (List.filter_map filter rem) 

(* Checks whether we can apply rule [a,b] i.e. for f = [a].mu, there exists [b].mu' where b != a        *)
(* If we cannot apply rule [a,b] then                                                                   *)
(*  Check whether there is any other rule that can be applied to the formulas in unseen apart from [a]  *)
(*  If there is no other rule, apply rule [a]                                                           *)

and apply_universal (f: Ast.Formula.Universal.t) (unseen: Ast.Formula.t list) (seen: Ast.Formula.t list): FormulaSet.t list * rule = 
  let check_ab = fun x -> 
    match x with 
    | Ast.Formula.Universal(y) -> (f.act <> y.act) 
    | _ -> false 
  in 

  if List.exists check_ab unseen 
  then ([ FormulaSet.singleton (create_verdict true) ], BoxAB) (* Rule [a,b] *)
  else (
    let inner_app = apply_rules unseen seen in 
    if (fst inner_app) = []
    then ([apply_universal_a f seen], BoxA {Ast.Act.name = f.act.name} ) (* Rule [a] *) 
    else inner_app (* Some other rule *)
  )

(* Function that checks whether a node with value f has already been created  *)
(* Returns a boolean value and the id of the node already visited             *)

let rec node_exists (f: FormulaSet.t) (visited: visitedNodes): int = 
  try snd (List.find (fun x -> fst x = f) visited) 
  with Not_found -> -1 

(* Function that sets which nodes are targets of back edges in two passes.                    *)
(* During the first pass, get the target id of all the leaves with back-edges.                *)
(* During the second pass, update the field back_edge of inner nodes to true accordingly.     *)

let rec set_backedge_targets (t: FormulaSet.t TabTree.tree): FormulaSet.t TabTree.tree = 
  
  let rec get_targets (t: FormulaSet.t TabTree.tree) (targets: int list): int list = 
    match t with 
    | TabTree.Leaf(x) -> if x.back_edge_target >= 0 then (targets @ [x.back_edge_target]) else targets
    | TabTree.Node(x) -> List.fold_left (fun acc y -> acc @ (get_targets y targets)) targets x.children
  
  and inner_set (t: FormulaSet.t TabTree.tree) (targets: int list): FormulaSet.t TabTree.tree =
    match t with 
    | TabTree.Leaf(x) -> t
    | TabTree.Node(x) -> 
      if List.mem x.id targets
      then TabTree.create_node x.id x.node_value (List.map (fun y -> inner_set y targets) x.children) x.rule true
      else TabTree.create_node x.id x.node_value (List.map (fun y -> inner_set y targets) x.children) x.rule false
  
  in inner_set t (get_targets t []) 

(* This function create a tableau using two main functions.                                                        *)
(* The function create_tableau takes two parameters; a set of formulas and a list visited.                         *)
(* The list visited stores the nodes created when creating a tableau together with the resp id of those nodes.     *)
(* If the set of formulas is a singleton of a verdict, then create a leaf node.                                    *)
(* Else, call the second main function inner_create_tableau.                                                       *)
(* The function inner_create_tableau decides whether to apply a rule or to add a back-edge.                        *)
(* If the set of formulas have already been visited, then there is a back-edge.                                    *)
(* Else, add the current set of formulas to visited, apply the rules to derive the children, and create a node.    *)

let rec create_tableau (f: FormulaSet.t) (visited: visitedNodes): FormulaSet.t TabTree.tree =
  if FormulaSet.is_empty f
  then TabTree.create_leaf FormulaSet.empty (-1) 
  else if FormulaSet.cardinal f == 1 
  then (
    match FormulaSet.min_elt f with 
    | Ast.Formula.Verdict(y) -> TabTree.create_leaf f (-1)
    | _ -> inner_create_tableau f visited
  )
  else inner_create_tableau f visited

and inner_create_tableau (f: FormulaSet.t) (visited: visitedNodes): FormulaSet.t TabTree.tree =
  let target_id = node_exists f visited in
    if target_id >= 0
    then TabTree.create_leaf f target_id  
    else (
      let res = apply_rules (FormulaSet.elements f) [] in 
        let count = !nodeCounter in
          let visited = [(f, count)] @ visited in
            nodeCounter := !nodeCounter + 1;
            let children = List.map (fun x -> create_tableau x visited) (fst res) in
              TabTree.create_node count f children (snd res) false
    )

(* Function that first creates the tableau and then updates the inner nodes that are targets of back-edges. *)
and formula_to_tableau (f: FormulaSet.t) =
  let t = create_tableau f [] 
  in set_backedge_targets t 

let rec formula_is_tt (f: Ast.Formula.t): bool =
  match f with 
  | Ast.Formula.Verdict(x) -> x.verdict
  | _ -> false

and formula_is_lvar (f: Ast.Formula.t): bool = 
  match f with 
  | Ast.Formula.LVar(x) -> true
  | _ -> false



let rec relabel_tableau (t: FormulaSet.t TabTree.tree): FormulaSet.t TabTree.tree = 
  match t with 
  | TabTree.Node(x) ->      
      if x.back_edge  
      then 
        let new_children = List.map (fun y -> relabel_tableau y) x.children in  
        let cont = (
          match List.hd new_children with
          | TabTree.Leaf(y) -> FormulaSet.choose y.leaf_value
          | TabTree.Node(y) -> FormulaSet.choose y.node_value
        ) in
        let max_formula = create_max {Ast.Formula.LVar.lvar = ("X" ^ (string_of_int x.id))} cont
        in TabTree.create_node x.id (FormulaSet.singleton max_formula) new_children x.rule x.back_edge
      else (
        match x.rule with 
        | BoxAB | Disjunction | Max | X | TT | FF -> inherit_children x
        | Conjunction -> conjunct_children x
        | BoxA(a) -> universal_children x a
        | _ -> t
      )

  | TabTree.Leaf(x) -> 
    let relabel_leaf = 
      if x.back_edge_target >= 0 
      then create_lvar ("X" ^ (string_of_int x.back_edge_target))
      else if FormulaSet.exists (fun x -> formula_is_tt x) x.leaf_value
      then create_verdict true
      else create_verdict false
    in TabTree.create_leaf (FormulaSet.singleton relabel_leaf) x.back_edge_target


and inherit_children (n: FormulaSet.t TabTree.node) = 
  let new_children = List.map (fun y -> relabel_tableau y) n.children in 
    let new_node_value =  
      match List.hd new_children with  (* because we know for sure that this only has one child i.e. children is one list *)
      | TabTree.Leaf(x) -> x.leaf_value 
      | TabTree.Node(x) -> x.node_value
    in TabTree.create_node n.id new_node_value new_children n.rule n.back_edge

and conjunct_children (n: FormulaSet.t TabTree.node) = 
  let new_children = List.map (fun y -> relabel_tableau y) n.children in 
    let new_node_value =  
      (* we know for sure that this node has two children *)
      match ((List.nth new_children 0), (List.nth new_children 1)) with 
      | (TabTree.Leaf(x), TabTree.Leaf(y)) -> create_conjunction (FormulaSet.choose x.leaf_value) (FormulaSet.choose y.leaf_value) 
      | (TabTree.Leaf(x), TabTree.Node(y)) -> create_conjunction (FormulaSet.choose x.leaf_value) (FormulaSet.choose y.node_value)
      | (TabTree.Node(x), TabTree.Leaf(y)) -> create_conjunction (FormulaSet.choose x.node_value) (FormulaSet.choose y.leaf_value)
      | (TabTree.Node(x), TabTree.Node(y)) -> create_conjunction (FormulaSet.choose x.node_value) (FormulaSet.choose y.node_value)
    in TabTree.create_node n.id (FormulaSet.singleton new_node_value) new_children n.rule n.back_edge
    
and universal_children (n: FormulaSet.t TabTree.node) (a: Ast.Act.t) = 
let new_children = List.map (fun y -> relabel_tableau y) n.children in 
  let new_node_value =  
    match List.hd new_children with  (* because we know for sure that this only has one child i.e. children is one list *)
    | TabTree.Leaf(x) -> create_universal a (FormulaSet.choose x.leaf_value) 
    | TabTree.Node(x) -> create_universal a (FormulaSet.choose x.node_value)
  in TabTree.create_node n.id (FormulaSet.singleton new_node_value) new_children n.rule n.back_edge

let rec rule_to_string (rule: rule): string = 
  match rule with 
  | Disjunction -> "or"
  | Conjunction -> "and"
  | Max -> "max"
  | BoxA(x) -> "[" ^ x.name ^ "]" 
  | BoxAB -> "[a,b]"
  | X -> "X"
  | TT -> "tt"
  | FF -> "ff"
  | None -> ""


(* Given a tree, print its tree representation *)
let rec print_tab_tree (input: FormulaSet.t TabTree.tree) (tab: string) = 

  let rec print_leaf (l: FormulaSet.t TabTree.leaf) (tab: string) = 
    " " ^ List.fold_left (fun acc x -> acc ^ (formula_to_string x) ^ "; " ) "" (FormulaSet.elements l.leaf_value) 
    ^ (if l.back_edge_target >= 0 then " back edge to " ^ string_of_int l.back_edge_target else "")

  and print_node (n: FormulaSet.t TabTree.node) (tab: string) = 
    (* Print the node value *)
    let value_string = List.fold_left (fun acc x -> acc ^ (formula_to_string x) ^ "; " ) "" (FormulaSet.elements n.node_value) in

      (* Print the children *)
      let rec get_children_string (children: FormulaSet.t TabTree.tree list) (tab: string): string = 
        match children with
        | [] -> ""
        | x::[] -> tab ^ "└─" ^ (print_tab_tree x tab)    
        | x::xs -> 
          tab ^ "├─" ^ 
          (print_tab_tree x (tab ^ "|") ) ^ "\n" ^
          (get_children_string xs tab)

    in 
    " (" ^ (rule_to_string n.rule) ^ ")" ^ (string_of_int n.id) ^ " ──── " ^ value_string 
    ^ (if n.back_edge then " back edge target\n" else "\n") 
    ^ (get_children_string n.children (tab ^ "  "))
        
  
  in match input with
  | TabTree.Leaf(x) -> print_leaf x tab
  | TabTree.Node(x) -> print_node x tab 