{
    open Parser
}

let ident = ['A' - 'Z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*
let symbol = ['a'-'z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*
let intlit = '-'? ['0'-'9']+

rule token = parse
    | eof { EOF }
    | [ ' ' '\t' '\n' ] { token lexbuf }
    | '#' [^'\n']* '\n' { token lexbuf }
    | "true" { BOOL true }
    | "false" { BOOL false }
    | intlit as i { INT (int_of_string i) }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACK }
    | "]" { RBRACK }
    | ";" { SEMI }
    | "," { COMMA }
    | "&&" { AND }
    | "||" { OR }
    | "=>" { IMPLIES }
    | "=" { EQUALS }
    | ":=" { DECL }
    | "!" { NOT }
    | "flag" { FLAG }
    | "has" { HAS }
    | "at" { AT }
    | "reachable" { REACHABLE }
    | "if" { IF }
    | "else" { ELSE }
    | "elif" { ELIF }
    | "item" { ITEM }
    | "location" { LOCATION }
    | "allowed_locations" { ALLOWED_LOCATIONS }
    | "constraint" { CONSTRAINT }
    | "region" { REGION }
    | ident as name { IDENT name }
    | symbol as name { SYMBOL name }
