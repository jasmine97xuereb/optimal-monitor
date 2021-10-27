open EnvFunctions
open PrettyPrint
open Ast

(* A user-defined type to desribe the tableau rules     *)
(* The rule None signifies that no rule can be applied  *)

type rule = Dijsunction | Conjunction | Max | BoxA | BoxAB | X | TT | FF | None

(* A tree can either be a leaf or a node            *)
(* A leaf contains some value                       *)
(* A node contains:                                 *)
(*  (i) some value                                  *)
(*  (ii) a list of children where children are tree *)
(*  (iii) the rule applied to derive the children   *)

module TabTree = 
  struct
    type 'a tree = 
      | Leaf of 'a leaf
      | Node of 'a node
    
    and 'a leaf = {
      leaf_value: 'a
    }
    and 'a node = { 
      node_value: 'a; 
      children: 'a tree list;
      rule: rule
    }
    let rec create_node (v: 'a) (c: 'a tree list) (r: rule) =
      Node {
        node_value = v;
        children = c;
        rule = r
      }
    let rec create_leaf (v: 'a) =
      Leaf {
        leaf_value = v
      }
  end;;

(* For each node n and its children, there is a rule st n is labelled with the premise and the children with its conclusion                 *)
(* The tableu rule is the rule [a] only if no other rule can be applied                                                                     *)
(* The function apply_rules takes a formula list, applies a rule, and returns the conclusion and the rule applied                           *)
(* Note that this function returns a list of Formula lists because the conclusion of rule [a] is two lists, each representing a tree branch *)

let rec apply_rules (unseen: Ast.Formula.t list) (seen: Ast.Formula.t list): Ast.Formula.t list list * rule = 
  match unseen with 
  | [] -> ([], None)
  | f::fs -> (
    match f with 
    | Ast.Formula.Verdict(x) -> apply_verdict x (fs @ seen)
    | Ast.Formula.Disjunction(x) -> ([apply_disjunction x (fs @ seen)], Dijsunction)
    | Ast.Formula.Conjunction(x) -> (apply_conjunction x (fs @ seen), Conjunction)
    | Ast.Formula.Max(x) -> ([apply_max x (fs @ seen)], Max)
    | Ast.Formula.LVar(x) -> ([[f]], X) (* This has to be fixed because we should unfold *)
    | Ast.Formula.Universal(x) -> apply_universal x fs ([f] @ seen)
    | _ -> ([], None)
  ) 

and apply_verdict (f: Ast.Formula.Verdict.t) (rem: Ast.Formula.t list): Ast.Formula.t list list * rule = 
  if f.verdict 
  then ([[create_verdict f.verdict]], TT) 
  else ([rem], FF)

and apply_disjunction (f: Ast.Formula.Disjunction.t) (rem: Ast.Formula.t list): Ast.Formula.t list =
  [f.left] @ [f.right] @ rem  

and apply_conjunction (f: Ast.Formula.Conjunction.t) (rem: Ast.Formula.t list): Ast.Formula.t list list =
  [[f.left] @ rem] @ [[f.right] @ rem]

and apply_max (f: Ast.Formula.Max.t) (rem: Ast.Formula.t list): Ast.Formula.t list =
  rem @ [f.cont] 

(* Apply rule [a] *)
and apply_universal_a (f: Ast.Formula.Universal.t) (rem: Ast.Formula.t list): Ast.Formula.t list = 
  let filter = fun x -> match x with
  | Ast.Formula.Universal(y) -> if y.act = f.act then Some y.cont else None 
  | _ -> None
  in (List.filter_map filter rem) 

(* Checks whether we can apply rule [a,b] i.e. for f = [a].mu, there exists [b].mu' where b != a        *)
(* If we cannot apply rule [a,b] then                                                                   *)
(*  Check whether there is any other rule that can be applied to the formulas in unseen apart from [a]  *)
(*  If there is no other rule, apply rule [a]                                                           *)

and apply_universal (f: Ast.Formula.Universal.t) (unseen: Ast.Formula.t list) (seen: Ast.Formula.t list): Ast.Formula.t list list * rule = 
  let check_ab = fun x -> 
    match x with 
    | Ast.Formula.Universal(y) -> (f.act <> y.act) 
    | _ -> false 
  in 

  if List.exists check_ab unseen 
  then ([[create_verdict true]], BoxAB) (* Rule [a,b] *)
  else 
    let inner_app = apply_rules unseen seen in 
    if fst inner_app = []
    then ([apply_universal_a f seen], BoxA) (* Rule [a] *)
    else inner_app (* Some other rule *)

(* Returns a tree where each node is a list of formulas *)
let rec create_tableau (f: Ast.Formula.t list): Ast.Formula.t list TabTree.tree = 
  match f with 
  | [] -> TabTree.create_leaf [] 
  | x::[] -> (
    match x with 
    | Ast.Formula.Verdict(y) -> TabTree.create_leaf f 
    | Ast.Formula.LVar(y) -> TabTree.create_leaf f
    | _ -> 
      let res = apply_rules f [] in
        let children = List.map (fun x -> create_tableau x) (fst res) in
          TabTree.create_node f children (snd res)  
    )
  | _ ->
    let res = apply_rules f [] in
      let children = List.map (fun x -> create_tableau x) (fst res) in
        TabTree.create_node f children (snd res)

let rec rule_to_string (rule: rule): string = 
  match rule with 
  | Dijsunction -> "or"
  | Conjunction -> "and"
  | Max -> "max"
  | BoxA -> "[a]"
  | BoxAB -> "[a,b]"
  | X -> "X"
  | TT -> "tt"
  | FF -> "ff"
  | None -> ""

(* Given a tree, print its tree representation *)
let rec print_tab_tree (input: Ast.Formula.t list TabTree.tree) (tab: string) = 

  let rec print_leaf (l: Ast.Formula.t list TabTree.leaf) (tab: string) = 
    List.fold_left (fun acc x -> acc ^ (formula_to_string x) ^ "; " ) "" l.leaf_value

  and print_node (n: Ast.Formula.t list TabTree.node) (tab: string) = 
    (* Print the node value *)
    let value_string = List.fold_left (fun acc x -> acc ^ (formula_to_string x) ^ "; " ) "" n.node_value ^ "\n" in

      (* Print the children *)
      let rec get_children_string (children: Ast.Formula.t list TabTree.tree list) (tab: string): string = 
        match children with
        | [] -> ""
        | x::[] -> tab ^ "└─" ^ (print_tab_tree x tab)    
        | x::xs -> 
          tab ^ "├─" ^ 
          (print_tab_tree x (tab ^ "|") ) ^ "\n" ^
          (get_children_string xs tab)

    in " (" ^ (rule_to_string n.rule) ^ ") ──── " ^ value_string ^ (get_children_string n.children (tab ^ "  ")) 
  
  in match input with
  | TabTree.Leaf(x) -> print_leaf x tab
  | TabTree.Node(x) -> print_node x tab 