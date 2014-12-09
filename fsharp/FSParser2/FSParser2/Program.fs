open AST

open Microsoft.FSharp.Text.Lexing

open System
open System.IO

let parseExpression text = 
    let lexbuf      = LexBuffer<char>.FromString text
    let tokenStream = Lexer.tokenstream
    let expr        = Parser.start tokenStream lexbuf

    expr

[<EntryPoint>]
let main argv = 
   
    let failures = ref 0 
    let inc s i = 
        s := !s + i
    
    let print (cc : ConsoleColor) (msg : string) =
        let old = Console.ForegroundColor
        Console.ForegroundColor <- cc
        try
            Console.WriteLine msg
        finally
            Console.ForegroundColor <- old

    let fail (msg : string) = 
        inc failures 1
        print ConsoleColor.Red  <| sprintf "FAILED  : %s" msg
            
    let info (msg : string) = 
        print ConsoleColor.Gray <| sprintf "INFO    : %s" msg

    let hilight (msg : string) = 
        print ConsoleColor.White<| sprintf "HILIGHT : %s" msg

    let success (msg : string) = 
        print ConsoleColor.Green<| sprintf "SUCCESS : %s" msg

    let preludes    = [|"#if "; "#elif "|]
    let epilogues   = [|""; " // Testing"|]


    let ONE     = Id "ONE"
    let TWO     = Id "TWO"
    let THREE   = Id "THREE"
    let TrueId  = Id "true"
    let FalseId = Id "false"

    let inline (!!)  e   = Not(e)
    let inline (&&&) l r = And(l,r)
    let inline (|||) l r = Or(l,r)

    hilight "Testing Parser"

    info "Running positive test cases"

    let positiveTests = 
        [|
            "ONE"                       , ONE
            "ONE//"                     , ONE
            "ONE // Comment"            , ONE
            "!ONE"                      , !!ONE
            "!!ONE"                     , !! (!!ONE)
            "DEBUG"                     , (Id "DEBUG")
            "!DEBUG"                    , !! (Id "DEBUG")
            "O_s1"                      , Id "O_s1"
            "(ONE)"                     , (ONE)
            "ONE&&TWO"                  , ONE &&& TWO 
            "ONE||TWO"                  , ONE ||| TWO
            "( ONE && TWO )"            , ONE &&& TWO
            "ONE && TWO && THREE"       , (ONE &&& TWO) &&& THREE
            "ONE || TWO || THREE"       , (ONE ||| TWO) ||| THREE
            "ONE || TWO && THREE"       , ONE ||| (TWO &&& THREE)
            "ONE && TWO || THREE"       , (ONE &&& TWO) ||| THREE
            "ONE || (TWO && THREE)"     , ONE ||| (TWO &&& THREE)
            "ONE && (TWO || THREE)"     , ONE &&& (TWO ||| THREE)
            "!ONE || TWO && THREE"      , (!!ONE) ||| (TWO &&& THREE)
            "ONE && !TWO || THREE"      , (ONE &&& (!!TWO)) ||| THREE
            "ONE || !(TWO && THREE)"    , ONE ||| (!!(TWO &&& THREE))
            "true"                      , TrueId
            "false"                     , FalseId
        |]

    for test,expected in positiveTests do
        for prelude in preludes do
            let test = prelude + test
            for epilogue in epilogues do
                let test = test + epilogue
                try
                    let expr = parseExpression test
                    if expected <> expr then
                        fail <| sprintf "'%s', expected %A, actual %A" test expected expr
                with 
                | e -> fail <| sprintf "'%s', expected %A, actual %s,%A" test expected (e.GetType().Name) e.Message

    info "Running negative test cases"

    let negativeTests = 
        [|
            ""   
            "!"   
            "@"
            "&&" 
            "||" 
            "ONE@"
            "@ONE"
            "ONE!"
            "(ONE"
            "ONE)"
            "ONE&&"
            "ONE ||"
            "&& ONE"
            "||ONE"
            "ONE /* Comment"
            "ONE )(@$&%*@^#%#!$)"
        |]

    for test in negativeTests do
        for prelude in preludes do
            let test = prelude + test
            for epilogue in epilogues do
                let test = test + epilogue
                try
                    let expr = parseExpression test
                    fail <| sprintf "'%s', expected 'parse error', actual %A" test expr
                with 
                | e -> ()

    if !failures = 0 then
        success "All tests completed successfully"
        0
    else 
        fail <| sprintf "%d failure(s) detected"  !failures
        999




