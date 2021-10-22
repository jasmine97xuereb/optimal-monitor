open Str
open Lexing
open PrettyPrint
open StrongestMonCons

let parse_formula s = Parser.rechml Lexer.token (from_string s)

let main = 

  let input = (Sys.argv.(1) ^ "\n") in 
    let formula = 
      try parse_formula input 
      with _ ->   (* handle all possible exceptions *) 
        print_endline("There seems to be some problem parsing your formula! Make sure that you used proper bracketing!"); 
        exit 0;
    in 
    
    print_endline(pretty_print_ast formula 0);
    pretty_print_formula formula;
    get_strongest_mon_cons formula