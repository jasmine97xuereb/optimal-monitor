/* The parser has two sections.  */
/* The first section is for declarations, including token and type specifications, precedence directives, and other output directives. */
/* The second section is for specifying the grammar of the language to be parsed. */

/* First Part */

%{
open Ast

let action (a: string) = {
    Act.name = a;
  }

let lvar (x: string) = {
    Formula.LVar.lvar = x;
  }

let verdict (x: string) = 
  match x with 
  | "t" -> { Formula.Verdict.verdict = true } 
  | "f" -> { Formula.Verdict.verdict = false }
  | _ -> raise Not_found

let disjunction (l: Ast.Formula.t) (r: Ast.Formula.t) = Formula.Disjunction {
    Formula.Disjunction.left = l;
    Formula.Disjunction.right = r;
  }

let conjunction (l: Ast.Formula.t) (r: Ast.Formula.t) = 
  Formula.Conjunction {
    Formula.Conjunction.left = l;
    Formula.Conjunction.right = r;
  }

let existential (act: Ast.Act.t) (cont: Ast.Formula.t) = Formula.Existential {
  Formula.Existential.act = act;
  Formula.Existential.cont = cont;
}

let universal (act: Ast.Act.t) (cont: Ast.Formula.t) = Formula.Universal {
  Formula.Universal.act = act;
  Formula.Universal.cont = cont;
}
 
let min (lvar: Ast.Formula.LVar.t) (cont: Ast.Formula.t) = Formula.Min {
  Formula.Min.lvar = lvar;
  Formula.Min.cont = cont;
}

let max (lvar: Ast.Formula.LVar.t) (cont: Ast.Formula.t) = Formula.Max {
  Formula.Max.lvar = lvar;
  Formula.Max.cont = cont;
}

%}

%token <string> LVAR
%token <string> VAR
%token DOT
%token AND
%token OR
%token MIN
%token MAX
%token OP_ROUND CLS_ROUND
%token OP_ANGLE CLS_ANGLE
%token OP_BOX CLS_BOX
%token EOL

/* Associativity and Precedence directives. */
/* %left indicates left associativity. */
/* Precedence directives lower in the file have higher priority.  */
/* Note that we want OR/AND to have lower precendence than <>/[] because <a>.t | t needs to be parsed as (<a>.t) | (t) */
/* If we switch the priority around, we end up with <a>.(t | t)  */

%left OP_ROUND
%left AND OR
%left OP_BOX OP_ANGLE MIN MAX

%start rechml
%type <Ast.Formula.t> rechml

/* Second Part */
%% 

rechml:
  f = formula EOL { f }
;

formula:
  | verdict         {Formula.Verdict($1)}
  | lvar            {Formula.LVar($1)}
  | disjunction     {$1} 
  | conjunction     {$1}
  | existential     {$1}
  | universal       {$1}
  | min             {$1}
  | max             {$1} 
;

verdict:
  | v = VAR                                                               {verdict v} 
; 

lvar:
  | x = LVAR                                                              {lvar x}
;

disjunction:
  | left=formula OR right=formula                                         %prec OR        {disjunction left right}
  | left=formula OR OP_ROUND right=formula CLS_ROUND                      %prec OP_ROUND  {disjunction left right}
  | OP_ROUND left=formula CLS_ROUND OR right=formula                      %prec OP_ROUND  {disjunction left right}
  | OP_ROUND left=formula CLS_ROUND OR OP_ROUND right=formula CLS_ROUND   %prec OP_ROUND  {disjunction left right}
;

conjunction:
  | left=formula AND right=formula                                        %prec AND       {conjunction left right}
  | left=formula AND OP_ROUND right=formula CLS_ROUND                     %prec OP_ROUND  {conjunction left right}
  | OP_ROUND left=formula CLS_ROUND AND right=formula                     %prec OP_ROUND  {conjunction left right}
  | OP_ROUND left=formula CLS_ROUND AND OP_ROUND right=formula CLS_ROUND  %prec OP_ROUND  {conjunction left right}
;

existential:
  | OP_ANGLE act=action CLS_ANGLE cont=formula                            %prec OP_ANGLE  {existential act cont}
;

universal:
  | OP_BOX act=action CLS_BOX cont=formula                                %prec OP_BOX    {universal act cont}
;

min:
  | MIN lvar=lvar DOT cont=formula                                        %prec MIN       {min lvar cont}
  | MIN lvar=lvar DOT OP_ROUND cont=formula CLS_ROUND                     %prec MIN       {min lvar cont}
;

max:
  | MAX lvar=lvar DOT cont=formula                                        %prec MAX       {max lvar cont}
  | MAX lvar=lvar DOT OP_ROUND cont=formula CLS_ROUND                     %prec MAX       {max lvar cont}
;

action: 
  | act = VAR                                                                             {action act} 
;