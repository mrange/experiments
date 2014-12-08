module AST

type Expr =
    |   And of Expr*Expr
    |   Or  of Expr*Expr
    |   Not of Expr
    |   Id  of string

let rec eval (lookup : string -> bool) = function
    | And (l,r) -> (eval lookup l) && (eval lookup r)
    | Or (l,r)  -> (eval lookup l) || (eval lookup r)
    | Not e     -> not (eval lookup e)
    | Id id     -> lookup id

