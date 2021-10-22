{
  open Parser
}

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
  |bletter+as lxm                 {LVAR(lxm)}
  |sletter+as lxm                 {VAR(lxm)}
  |'\n'                           {EOL}
