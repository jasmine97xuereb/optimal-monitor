open PrettyPrint

let create_act (act: string): Ast.Act.t = 
  {Ast.Act.name = act}

let create_verdict (v: bool): Ast.Formula.t = 
  Ast.Formula.Verdict{Ast.Formula.Verdict.verdict = v}

let create_lvar (lvar: string): Ast.Formula.t = 
  Ast.Formula.LVar{lvar}

let create_disjunction (l: Ast.Formula.t) (r: Ast.Formula.t): Ast.Formula.t = 
  Ast.Formula.Disjunction {
    Ast.Formula.Disjunction.left = l;
    Ast.Formula.Disjunction.right = r;
  }

let create_conjunction (l: Ast.Formula.t) (r: Ast.Formula.t): Ast.Formula.t = 
  Ast.Formula.Conjunction {
    Ast.Formula.Conjunction.left = l;
    Ast.Formula.Conjunction.right = r;
  }

let create_existential (act: Ast.Act.t) (cont: Ast.Formula.t): Ast.Formula.t = 
  Ast.Formula.Existential {
    Ast.Formula.Existential.act = act;
    Ast.Formula.Existential.cont = cont;
  }

let create_universal (act: Ast.Act.t) (cont: Ast.Formula.t): Ast.Formula.t = 
  Ast.Formula.Universal {
    Ast.Formula.Universal.act = act;
    Ast.Formula.Universal.cont = cont;
  }
 
let create_min (lvar: Ast.Formula.LVar.t) (cont: Ast.Formula.t): Ast.Formula.t = 
  Ast.Formula.Min {
    Ast.Formula.Min.lvar = lvar;
    Ast.Formula.Min.cont = cont;
  }

let create_max (lvar: Ast.Formula.LVar.t) (cont: Ast.Formula.t): Ast.Formula.t = 
  Ast.Formula.Max {
    Ast.Formula.Max.lvar = lvar;
    Ast.Formula.Max.cont = cont;
  } 


(* Function that takes a formula and attempts to simplify it using the Laws of Boolean Algebra *)
(* We only check for two laws: Annulment Law & Identity Law                                    *)
(* Annulment Law: A and false = false, A or true = true                                        *)
(* Identity Law: A and true = true, A or false = A                                             *)

let rec apply_laws (f: Ast.Formula.t): Ast.Formula.t = 
  match f with 
  | Ast.Formula.Verdict(x) -> f
  | Ast.Formula.LVar(x) -> f 
  | Ast.Formula.Disjunction(x) -> apply_laws_disjunction x.left x.right
  | Ast.Formula.Conjunction(x) -> apply_laws_conjunction x.left x.right 
  | Ast.Formula.Existential(x) -> create_existential x.act (apply_laws x.cont)
  | Ast.Formula.Universal(x) -> create_universal x.act (apply_laws x.cont)
  | Ast.Formula.Min(x) -> create_min x.lvar (apply_laws x.cont)
  | Ast.Formula.Max(x) -> create_max x.lvar (apply_laws x.cont)

and apply_laws_disjunction (l: Ast.Formula.t) (r: Ast.Formula.t): Ast.Formula.t = 
  match (l, r) with 
  | (Ast.Formula.Verdict(x), Ast.Formula.Verdict(y)) -> 
    if x.verdict || y.verdict 
    then create_verdict true
    else create_verdict false
  | (Ast.Formula.Verdict(x), _) -> 
    if x.verdict 
    then create_verdict true 
    else apply_laws r 
  | (_, Ast.Formula.Verdict(y)) -> 
    if y.verdict 
    then create_verdict true 
    else apply_laws l 
  | (_, _) -> create_disjunction (apply_laws l) (apply_laws r)

and apply_laws_conjunction (l: Ast.Formula.t) (r: Ast.Formula.t): Ast.Formula.t = 
  match (l, r) with 
  | (Ast.Formula.Verdict(x), Ast.Formula.Verdict(y)) -> 
    if x.verdict && y.verdict 
    then create_verdict true
    else create_verdict false
  | (Ast.Formula.Verdict(x), _) -> 
    if x.verdict 
    then apply_laws r 
    else create_verdict false 
  | (_, Ast.Formula.Verdict(y)) -> 
    if y.verdict 
    then apply_laws l 
    else create_verdict false 
  | (_, _) -> create_conjunction (apply_laws l) (apply_laws r)