{
  open Parser
}

let digit = ['0'-'9']
let sletter = ['a'-'z']
let bletter = ['A'-'Z']
let whitespace = [' ' '\t' '\r']

rule token = parse
  whitespace+                     {token lexbuf}
  |'('                            {OP_ROUND}
  |')'                            {CLS_ROUND}
  |'<'                            {OP_ANGLE}
  |'>'                            {CLS_ANGLE}
  |'['                            {OP_BOX}
  |']'                            {CLS_BOX}
  |'.'                            {DOT}
  |'&'                            {AND}
  |'|'                            {OR}
  |"min"                          {MIN}
  |"max"                          {MAX}
  |"tt"                           {TT}
  |"ff"                           {FF}
  |bletter(digit)* as lxm         {LVAR(lxm)}
  |sletter(sletter|digit)* as lxm {VAR(lxm)}
  |'\n'                           {EOL}