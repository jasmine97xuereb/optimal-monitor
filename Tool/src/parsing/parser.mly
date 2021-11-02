/* The parser has two sections.  */
/* The first section is for declarations, including token and type specifications, precedence directives, and other output directives. */
/* The second section is for specifying the grammar of the language to be parsed. */

/* First Part */

%{
  open Ast
%}

%token <string> LVAR
%token <string> VAR
%token TT FF
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

%left MIN MAX
%left OP_ROUND
%left AND OR
%left OP_BOX OP_ANGLE 

%start rechml
%type <Ast.formula> rechml

/* Second Part */
%% 

rechml:
  f = formula EOL { f }
;

formula:
  | tt              {$1}
  | ff              {$1}
  | lvar            {LVar($1)}
  | disjunction     {$1} 
  | conjunction     {$1}
  | existential     {$1}
  | universal       {$1}
  | min             {$1}
  | max             {$1}
;

tt:
  | TT                                                                    {TT}
;

ff:
  | FF                                                                    {FF} 
; 

lvar:
  | x = LVAR                                                              {x}
;

disjunction:
  | left=formula OR right=formula                                         %prec OR        {Disjunction (left, right)}
  | OP_ROUND left=formula OR right=formula CLS_ROUND                      %prec OR        {Disjunction (left, right)}
  | left=formula OR OP_ROUND right=formula CLS_ROUND                      %prec OP_ROUND  {Disjunction (left, right)}
  | OP_ROUND left=formula CLS_ROUND OR right=formula                      %prec OP_ROUND  {Disjunction (left, right)}
  | OP_ROUND left=formula CLS_ROUND OR OP_ROUND right=formula CLS_ROUND   %prec OP_ROUND  {Disjunction (left, right)}
;

conjunction:
  | left=formula AND right=formula                                        %prec AND       {Conjunction (left, right)}
  | OP_ROUND left=formula AND right=formula CLS_ROUND                     %prec AND       {Conjunction (left, right)}
  | left=formula AND OP_ROUND right=formula CLS_ROUND                     %prec OP_ROUND  {Conjunction (left, right)}
  | OP_ROUND left=formula CLS_ROUND AND right=formula                     %prec OP_ROUND  {Conjunction (left, right)}
  | OP_ROUND left=formula CLS_ROUND AND OP_ROUND right=formula CLS_ROUND  %prec OP_ROUND  {Conjunction (left, right)}
;

existential:
  | OP_ANGLE act=action CLS_ANGLE cont=formula                            %prec OP_ANGLE  {Existential (act, cont)}
;

universal:
  | OP_BOX act=action CLS_BOX cont=formula                                %prec OP_BOX    {Universal (act, cont)}
;

min:
  | MIN lvar=lvar DOT cont=formula                                        %prec MIN       {Min (lvar, cont)}
  | MIN lvar=lvar DOT OP_ROUND cont=formula CLS_ROUND                     %prec MIN       {Min (lvar, cont)}
;

max:
  | MAX lvar=lvar DOT cont=formula                                        %prec MAX       {Max (lvar, cont)}
  | MAX lvar=lvar DOT OP_ROUND cont=formula CLS_ROUND                     %prec MAX       {Max (lvar, cont)}
;

action: 
  | act = VAR                                                                             {act} 
;
