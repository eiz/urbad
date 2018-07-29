open Printf

type decl =
    | Dpredicate of string * expr
    | Dflag of string * bool
    | Ditem of string * int option
    | Dlocation of string * expr
    | Dallowed of string list * string list
    | Dconstraint of expr
    | Dregion of string * decl list
and expr =
    | Eand of expr * expr
    | Eor of expr * expr
    | Eimplies of expr * expr
    | Eequals of expr * expr
    | Eif of (expr * expr) list * expr option
    | Enot of expr
    | Epredicate of string
    | Esymbol of string
    | Eflag of string
    | Ehas of string * int option
    | Eat of string * string
    | Ereachable of string
    | Ebool of bool

let rec string_of_decl = function
    | Dpredicate(s, e) -> sprintf "Dpredicate(%s,%s)" s (string_of_expr e)
    | Dflag(s, b) -> sprintf "Dflag(%s,%B)" s b
    | Ditem(s, Some i) -> sprintf "Ditem(%s, Some %d)" s i
    | Ditem(s, None) -> sprintf "Ditem(%s, None)" s
    | Dlocation(l, e) -> sprintf "Dlocation(%s, %s)" l (string_of_expr e)
    | Dallowed(l, i) -> sprintf "Dallowed([%s], [%s])"
        (String.concat ", " l) (String.concat ", " i)
    | Dconstraint e -> sprintf "Dconstraint(%s)" (string_of_expr e)
    | Dregion(n, d) ->
        sprintf "Dregion(%s, [%s])" n
            (String.concat ", " (List.map string_of_decl d))
and string_of_expr = function
    | Eand(l, r) -> sprintf "Eand(%s,%s)" (string_of_expr l) (string_of_expr r)
    | Eor(l, r) -> sprintf "Eor(%s,%s)" (string_of_expr l) (string_of_expr r)
    | Eimplies(l, r) ->
        sprintf "Eimplies(%s,%s)" (string_of_expr l) (string_of_expr r)
    | Eequals(l, r) ->
        sprintf "Eequals(%s,%s)" (string_of_expr l) (string_of_expr r)
    | Eif(c, Some e) ->
        sprintf "Eif([%s], Some %s)" (string_of_conds c) (string_of_expr e)
    | Eif(c, None) ->
        sprintf "Eif([%s], None)" (string_of_conds c)
    | Enot e -> sprintf "Enot(%s)" (string_of_expr e)
    | Epredicate p -> sprintf "Epredicate(%s)" p
    | Esymbol s -> sprintf "Esymbol(%s)" s
    | Eflag f -> sprintf "Eflag(%s)" f
    | Ehas(s, Some i) -> sprintf "Ehas(%s, Some %d)" s i
    | Ehas(s, None) -> sprintf "Ehas(%s, None)" s
    | Eat(i, l) -> sprintf "Eat(%s,%s)" i l
    | Ereachable s -> sprintf "Ereachable(%s)" s
    | Ebool b -> sprintf "Ebool(%B)" b
and string_of_conds c =
    String.concat ", " (List.map
        (fun (l, r) -> sprintf "(%s,%s)"
            (string_of_expr l) (string_of_expr r)) c)
