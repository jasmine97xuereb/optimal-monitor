open EnvFunctions
open EnvResources
open PrettyPrint
open Ast
exception Foo of string

(* For each node n and its children, there is a rule st n is labelled with the premise and the children with its conclusion                 *)
(* The tableu rule is the rule [a] only if no other rule can be applied                                                                     *)
(* The function apply_rules takes a formula list, applies a rule, and returns the conclusion and the rule applied                           *)
(* Note that this function returns a list of Formula sets because the conclusion of rule [a] is two sets, each representing a tree branch   *)

let rec apply_rules (unseen: Ast.formula list) (seen: Ast.formula list): FormulaSet.t list * rule = 
  match unseen with 
  | [] -> ([], None)
  | f::fs -> (
    match f with 
    | TT -> ([FormulaSet.singleton f], TT)
    | FF -> ([FormulaSet.of_list (fs @ seen)], FF)
    | Disjunction(l, r) -> ([apply_disjunction l r (fs @ seen)], Disjunction)
    | Conjunction(l, r) -> (apply_conjunction l r (fs @ seen), Conjunction)
    | Max(_, cont)  -> ([apply_max cont (fs @ seen)], Max)
    | LVar(x) -> ( [ FormulaSet.of_list ( [LVars.find x !map] @ fs @ seen) ], X)
    | Universal(a, _) -> apply_universal a fs ([f] @ seen)
    | _ -> ([], None)
  ) 

and apply_disjunction (l: Ast.formula) (r: Ast.formula) (rem: Ast.formula list): FormulaSet.t =
  FormulaSet.of_list ([l] @ [r] @ rem)  

and apply_conjunction (l: Ast.formula) (r: Ast.formula) (rem: Ast.formula list): FormulaSet.t list =
  [ FormulaSet.of_list ([l] @ rem) ] @ [ FormulaSet.of_list ([r] @ rem) ]

and apply_max (cont: Ast.formula) (rem: Ast.formula list): FormulaSet.t =
  FormulaSet.of_list (rem @ [cont]) 

(* Apply rule [a] *)
and apply_universal_a (a: Ast.action) (rem: Ast.formula list): FormulaSet.t = 
  let filter = fun x -> 
    match x with
    | Universal(b, cont) -> if b = a then Some cont else None 
    | _ -> None
  in FormulaSet.of_list (List.filter_map filter rem) 

(* Checks whether we can apply rule [a,b] i.e. for f = [a].mu, there exists [b].mu' where b != a        *)
(* If we cannot apply rule [a,b] then                                                                   *)
(*  Check whether there is any other rule that can be applied to the formulas in unseen apart from [a]  *)
(*  If there is no other rule, apply rule [a]                                                           *)

and apply_universal (a: Ast.action) (unseen: Ast.formula list) (seen: Ast.formula list): FormulaSet.t list * rule = 
  let check_ab = fun x -> 
    match x with 
    | Universal(b, _) -> (a <> b) 
    | _ -> false 
  in 

  if List.exists check_ab unseen 
  then ([ FormulaSet.singleton TT ], BoxAB) (* Rule [a,b] *)
  else (
    let inner_app = apply_rules unseen seen in 
    if (fst inner_app) = []
    then ([apply_universal_a a seen], BoxA(a) ) (* Rule [a] *) 
    else inner_app (* Some other rule *)
  )

(* Function that checks whether a node with value f has already been created  *)
(* Returns a boolean value and the id of the node already visited             *)

let node_exists (f: FormulaSet.t) (visited: visitedNodes): int = 
  try snd (List.find (fun x -> fst x = f) visited) 
  with Not_found -> -1 

(* Function that sets which nodes are targets of back edges in two passes.                    *)
(* During the first pass, get the target id of all the leaves with back-edges.                *)
(* During the second pass, update the field back_edge of inner nodes to true accordingly.     *)

let set_backedge_targets (t: FormulaSet.t TabTree.tree): FormulaSet.t TabTree.tree = 
  
  let rec get_targets (t: FormulaSet.t TabTree.tree) (targets: int list): int list = 
    match t with 
    | TabTree.Leaf(x) -> if x.back_edge_target >= 0 then (targets @ [x.back_edge_target]) else targets
    | TabTree.Node(x) -> List.fold_left (fun acc y -> acc @ (get_targets y targets)) targets x.children
  
  and inner_set (t: FormulaSet.t TabTree.tree) (targets: int list): FormulaSet.t TabTree.tree =
    match t with 
    | TabTree.Leaf _ -> t
    | TabTree.Node(x) -> 
      if List.mem x.id targets
      then TabTree.create_node x.id x.node_value (List.map (fun y -> inner_set y targets) x.children) x.rule true
      else TabTree.create_node x.id x.node_value (List.map (fun y -> inner_set y targets) x.children) x.rule false
  
  in inner_set t (get_targets t []) 

(* This function create a tableau using two main functions.                                                        *)
(* The value at each node/leaf is a set of formulas.                                                               *)
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
    (* | TT | FF -> TabTree.create_leaf f (-1) *)
    | TT -> TabTree.create_leaf f (-1)
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

let formula_is_tt (f: Ast.formula): bool =
  match f with 
  | TT -> true
  | _ -> false

and formula_is_lvar (f: Ast.formula): bool = 
  match f with 
  | LVar _ -> true
  | _ -> false

(* Given a tableau, this function traverses it bottom up and creates a new disjunction free tableau.  *)  
(* The value at each node/leaf of the relabelled tableau is a single formula.                         *)
(* For each leaf node:                                                                                *)
(*  If it has a back-edge to a node with id n, then relabel the leaf to Xn.                           *)
(*  If it does not have a back-edge then if it contains tt, label it tt, else label it ff.            *)
(* For each non-leaf node with id m:                                                                  *)
(*    If node m has children with rules BoxAB, Disjunction, Max, X, TT, FF then inherit the children. *)
(*    If node m has children with rule BoxA, then inherit the children and prefix them with [a].      *)
(*    If node m has children with rule AND, then inherit the children and conjunct them.              *)
(*  If it is the target of a back-edge then label it max Xm.m' is the node label.                     *)

let rec relabel_tableau (t: FormulaSet.t TabTree.tree): Ast.formula TabTree.tree = 
  match t with 
  | TabTree.Leaf(x) -> 
    let relabel_leaf = 
      if x.back_edge_target >= 0 
      then LVar("X" ^ (string_of_int x.back_edge_target))
      else if FormulaSet.exists (fun x -> formula_is_tt x) x.leaf_value
      then TT
      else FF
    in TabTree.create_leaf relabel_leaf x.back_edge_target

  | TabTree.Node(x) ->      
    let new_formula = (
      match x.rule with 
      | BoxAB | Disjunction | Max | X | TT | FF | None -> inherit_children x
      | Conjunction -> conjunct_children x
      | BoxA(a) -> universal_children x a 
    ) in
    if x.back_edge  
      then 
        let children = try get_node_children new_formula with e -> raise e
        in let value = try get_node_value new_formula with e -> raise e 
        in let max_formula = Max ("X" ^ (string_of_int x.id), value)
        in TabTree.create_node x.id max_formula children x.rule x.back_edge
    else new_formula 
      

and inherit_children (n: FormulaSet.t TabTree.node): Ast.formula TabTree.tree = 
  let new_children = List.map (fun y -> relabel_tableau y) n.children in 
    let new_node_value = try get_child new_children with e -> raise e 
      in TabTree.create_node n.id new_node_value new_children n.rule n.back_edge

and universal_children (n: FormulaSet.t TabTree.node) (a: Ast.action): Ast.formula TabTree.tree = 
  let new_children = List.map (fun y -> relabel_tableau y) n.children in 
    let cont = try get_child new_children with e -> raise e in 
      let new_node_value = Universal(a, cont) in     
        TabTree.create_node n.id new_node_value new_children n.rule n.back_edge

and conjunct_children (n: FormulaSet.t TabTree.node): Ast.formula TabTree.tree = 
  let new_children = List.map (fun y -> relabel_tableau y) n.children in 
    let new_node_value =  
      (* If the node has more than two children after the relabelling, raise an exception.  *)
      if List.length new_children != 2
      then raise (Foo "Error in relabelling! The node has more than two children.")    
      else match ((List.nth new_children 0), (List.nth new_children 1)) with 
      | (TabTree.Leaf(x), TabTree.Leaf(y)) -> Conjunction(x.leaf_value, y.leaf_value) 
      | (TabTree.Leaf(x), TabTree.Node(y)) -> Conjunction(x.leaf_value, y.node_value)
      | (TabTree.Node(x), TabTree.Leaf(y)) -> Conjunction(x.node_value, y.leaf_value)
      | (TabTree.Node(x), TabTree.Node(y)) -> Conjunction(x.node_value, y.node_value)
    in TabTree.create_node n.id new_node_value new_children n.rule n.back_edge
    
(* This function takes a list of children and returns a single child value.   *)
(* If the there is more than one child, the function raises an exception.     *)
and get_child (n: Ast.formula TabTree.tree list): Ast.formula =
  if List.length n != 1 
  then raise (Foo "Error in relabelling! The node has more than one child.")
  else 
    match List.hd n with  
      | TabTree.Leaf(x) -> x.leaf_value 
      | TabTree.Node(x) -> x.node_value

and get_node_children (n: Ast.formula TabTree.tree): Ast.formula TabTree.tree list =
  match n with
    | TabTree.Node(y) -> y.children
    | _ -> raise (Foo "Error in relabelling! The node does not have any children!")

and get_node_value (n: Ast.formula TabTree.tree) =
  match n with
    | TabTree.Node(y) -> y.node_value
    | _ -> raise (Foo "Error in relabelling! The node does not have any children!")

let rule_to_string (rule: rule): string = 
  match rule with 
  | Disjunction -> "or"
  | Conjunction -> "and"
  | Max -> "max"
  | BoxA(a) -> "[" ^ a ^ "]" 
  | BoxAB -> "[a,b]"
  | X -> "X"
  | TT -> "tt"
  | FF -> "ff"
  | None -> ""

(* Given a tree where each node is a formula set, print its tree representation *)
let rec print_tab_tree (input: FormulaSet.t TabTree.tree) (tab: string): string = 

  let print_leaf (l: FormulaSet.t TabTree.leaf): string = 
    " " ^ List.fold_left (fun acc x -> acc ^ (formula_to_string x) ^ "; " ) "" (FormulaSet.elements l.leaf_value) 
    ^ (if l.back_edge_target >= 0 then " back edge to " ^ string_of_int l.back_edge_target else "")

  and print_node (n: FormulaSet.t TabTree.node) (tab: string): string = 
    let value_string = List.fold_left (fun acc x -> acc ^ (formula_to_string x) ^ "; " ) "" (FormulaSet.elements n.node_value) in
      let rec get_children_string (children: FormulaSet.t TabTree.tree list) (tab: string): string = 
        match children with
        | [] -> ""
        | x::[] -> tab ^ "└─" ^ (print_tab_tree x tab)    
        | x::xs -> tab ^ "├─" ^ (print_tab_tree x (tab ^ "|") ) ^ "\n" ^ (get_children_string xs tab)
    in " (" ^ (rule_to_string n.rule) ^ ")" ^ (string_of_int n.id) ^ " ──── " ^ value_string 
    ^ (if n.back_edge then " back edge target\n" else "\n") 
    ^ (get_children_string n.children (tab ^ "  "))
        
  in match input with
  | TabTree.Leaf(x) -> print_leaf x
  | TabTree.Node(x) -> print_node x tab 

(* Given a tree where each node is a formula, print its tree representation *)
let rec print_relabelled_tab (input: Ast.formula TabTree.tree) (tab: string): string = 

  let print_leaf (l: Ast.formula TabTree.leaf): string = 
    " " ^ formula_to_string l.leaf_value 
    ^ (if l.back_edge_target >= 0 then " back edge to " ^ string_of_int l.back_edge_target else "")

  and print_node (n: Ast.formula TabTree.node) (tab: string) = 
    let value_string = formula_to_string n.node_value in
      let rec get_children_string (children: Ast.formula TabTree.tree list) (tab: string): string = 
        match children with
        | [] -> ""
        | x::[] -> tab ^ "└─" ^ (print_relabelled_tab x tab)    
        | x::xs -> tab ^ "├─" ^ (print_relabelled_tab x (tab ^ "|") ) ^ "\n" ^ (get_children_string xs tab)
    in " (" ^ (rule_to_string n.rule) ^ ")" ^ (string_of_int n.id) ^ " ──── " ^ value_string 
    ^ (if n.back_edge then " back edge target\n" else "\n") 
    ^ (get_children_string n.children (tab ^ "  "))
  
  in match input with
  | TabTree.Leaf(x) -> print_leaf x
  | TabTree.Node(x) -> print_node x tab  