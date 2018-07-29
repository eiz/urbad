open Printf
open Ast

let rec print_list = function
    | hd :: tl -> printf "%s\n" hd; print_list tl
    | [] -> ()

let _ =
    let f = open_in "logic/nmg.logic" in
    let l = Lexing.from_channel f in
    let result = Parser.toplevel Lexer.token l in
    print_list (List.map string_of_decl result)
