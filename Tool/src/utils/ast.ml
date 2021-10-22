module rec Act : sig 
  type t = {
    name: string
  }
end = Act

and Formula : sig

  module Verdict: sig 
    type t = {
      verdict: bool
    }
  end

  module Disjunction : sig
    type t = {
      left: Formula.t;
      right: Formula.t;
    }
  end 

  module Conjunction : sig
    type t = {
      left: Formula.t;
      right: Formula.t;
    }
  end 

  module Existential : sig
    type t = {
      act: Act.t;
      cont: Formula.t;
    }
  end 
  
  module Universal : sig
    type t = {
      act: Act.t;
      cont: Formula.t;
    }
  end 

  module LVar : sig
    type t = {
      lvar: string
    }
  end

  module Min : sig
    type t = {
      lvar: LVar.t;
      cont: Formula.t;
    }
  end 

  module Max : sig
    type t = {
      lvar: LVar.t;
      cont: Formula.t;
    }
  end 

  type t = 
    | Verdict of Verdict.t
    | Disjunction of Disjunction.t
    | Conjunction of Conjunction.t
    | Existential of Existential.t
    | Universal of Universal.t
    | Min of Min.t
    | Max of Max.t
    | LVar of LVar.t

end = Formula


(* SYNTAX OF RECHML                               *)  
(* tt                  truth                      *)
(* ff                  falsehodd                  *)
(* f1 or f2            disjunction                *)
(* f2 and f2           conjunction                *)
(* <a>f                existential modality       *)
(* [a]f                universal modality         *)
(* min X.f             least fixpoint             *)
(* max X.f             greatest fixpoint          *)
(* X                   recursion variable         *)
