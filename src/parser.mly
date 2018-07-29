%{
    open Ast
%}
%token <string> IDENT SYMBOL
%token <bool> BOOL
%token <int> INT
%token LPAREN RPAREN SEMI AND OR IMPLIES EQUALS FLAG HAS REACHABLE ITEM COMMA
%token LOCATION ALLOWED_LOCATIONS CONSTRAINT DECL EOF NOT REGION LBRACE AT IF
%token ELIF ELSE RBRACE LBRACK RBRACK
%nonassoc NOT
%left IMPLIES
%left EQUALS
%left OR
%left AND
%start toplevel
%type <Ast.decl list> toplevel
%%

toplevel:
    decl_list EOF { List.rev $1 }
;

decl_list:
    decl_list decl { $2 :: $1 }
|   { [] }
;

decl:
    flag_decl SEMI { $1 }
|   item_decl SEMI { $1 }
|   location_decl SEMI { $1 }
|   allowed_locations_decl SEMI { $1 }
|   constraint_decl SEMI { $1 }
|   predicate_decl SEMI { $1 }
|   REGION SYMBOL LBRACE decl_list RBRACE { Dregion($2, List.rev $4) }
;

flag_decl:
    FLAG SYMBOL BOOL { Dflag($2, $3) }
;

item_decl:
    ITEM SYMBOL { Ditem($2, None) }
|   ITEM SYMBOL LBRACK INT RBRACK { Ditem($2, Some $4) }
;

location_decl:
    LOCATION SYMBOL DECL expr { Dlocation($2, $4) }
;

allowed_locations_decl:
    ALLOWED_LOCATIONS symbol_list DECL symbol_list {
        Dallowed(List.rev $2, List.rev $4)
    }
;

symbol_list:
    symbol_list SYMBOL { $2 :: $1 }
|   { [] }
;

constraint_decl:
    CONSTRAINT expr { Dconstraint $2 }
;

predicate_decl:
    IDENT DECL expr { Dpredicate($1, $3) }
;

expr:
    LPAREN expr RPAREN { $2 }
|   expr AND expr { Eand($1, $3) }
|   expr OR expr { Eor($1, $3) }
|   expr IMPLIES expr { Eimplies($1, $3) }
|   expr EQUALS expr { Eequals($1, $3) }
|   NOT expr { Enot($2) }
|   IDENT { Epredicate $1 }
|   SYMBOL { Esymbol $1 }
|   FLAG LPAREN SYMBOL RPAREN { Eflag $3 }
|   HAS LPAREN SYMBOL RPAREN { Ehas($3, None) }
|   HAS LPAREN SYMBOL COMMA INT RPAREN { Ehas($3, Some $5) }
|   AT LPAREN SYMBOL COMMA SYMBOL RPAREN { Eat($3, $5) }
|   REACHABLE LPAREN SYMBOL RPAREN { Ereachable $3 }
|   if_expr { $1 }
|   BOOL { Ebool $1 }
;

if_no_else_expr:
    IF LPAREN expr RPAREN LBRACE expr RBRACE { Eif([($3, $6)], None) }
;

if_elif_expr:
    if_elif_expr ELIF LPAREN expr RPAREN LBRACE expr RBRACE {
        match $1 with
          | Eif(l, _) -> Eif(($4, $7) :: l, None)
          | _ -> assert false
    }
|   if_no_else_expr { $1 }
;

if_expr:
    if_elif_expr ELSE LBRACE expr RBRACE {
        match $1 with
          | Eif(l, _) -> Eif(List.rev l, Some $4)
          | _ -> assert false
    }
|   if_elif_expr {
        match $1 with
          | Eif(l, None) -> Eif(List.rev l, None)
          | _ -> assert false
    }
