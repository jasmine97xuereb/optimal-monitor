open Map 
open Ast

(* A user-defined type to desribe the tableau rules     *)
(* The rule None signifies that no rule can be applied  *)

type rule = Disjunction | Conjunction | Max | BoxA of action | BoxAB | X | TT | FF | None

(* A tree can either be a leaf or a node                                    *)
(* A leaf contains:                                                         *)
(*  (i) some value                                                          *)
(*  (ii) the id of the back-edge target, -1 in case of no back-edge         *)
(* A node contains:                                                         *)
(*  (i) a unique id                                                         *)
(*  (ii) some value                                                         *)
(*  (iii) a list of children where children are tree                        *)
(*  (iv) the rule applied to derive the children                            *)
(*  (v) a boolean value indicating whether it is the target of a back-edge  *)

module TabTree = 
  struct
    type 'a tree = 
      | Leaf of 'a leaf
      | Node of 'a node
    
    and 'a leaf = {
      leaf_value: 'a;
      back_edge_target: int;
    }
    and 'a node = { 
      id: int;
      node_value: 'a; 
      children: 'a tree list;
      rule: rule;
      back_edge: bool;
    }
    let rec create_node (i: int) (v: 'a) (c: 'a tree list) (r: rule) (b: bool) =
      Node {
        id = i;
        node_value = v;
        children = c;
        rule = r;
        back_edge = b
      }
    let rec create_leaf (v: 'a) (b: int) =
      Leaf {
        leaf_value = v;
        back_edge_target = b
      }
  end;;


(* Map from LVars to Formulas used for min and max unfoldings *)
(* The key is an LVar *)
module LVars = Map.Make(struct type t = Ast.variable let compare = compare end)

(* Set of Formulas *)
module FormulaSet = Set.Make(struct type t = Ast.formula let compare = compare end)

let nodeCounter = ref 0

(* List of tuples where the first element is the node value and the second element is the node id. *)
type visitedNodes = (FormulaSet.t * int) list

