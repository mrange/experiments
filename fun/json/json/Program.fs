
module JSONParser = 
    open FParsec

    type JsonValue = 
    | String of string
    | Number of decimal
    | Float of float 
    | Record of properties:(string * JsonValue)[]
    | Array of elements:JsonValue[]
    | Boolean of bool
    | Null

    module Details =

        open System
        open System.Linq.Expressions

        let fastMap (items : (string*'T) list) (defaultValue : 'T) : Func<char, int, 'T> = 
            let tt = typeof<'T>
            let parameter0     = Expression.Parameter   (typeof<char>   , "ch"      )
            let parameter1     = Expression.Parameter   (typeof<int>    , "index"   )
            let resultVariable = Expression.Variable    (tt             , "result"  )

            let switchCases =
                items 
                |> List.map (fun (anyOf,v) -> 
                    Expression.SwitchCase    (
                        Expression.Assign     (resultVariable, Expression.Constant(v, tt))                  ,
                        anyOf |> Seq.map (fun ch -> Expression.Constant (ch) :> Expression) |> Seq.toArray) )
                |> List.toArray

            let switchStatement = 
                Expression.Switch (
                    parameter0                                                                          ,
                    Expression.Assign        (resultVariable, Expression.Constant(defaultValue, tt))    ,
                    switchCases                                                                         )

            let body = 
                Expression.Block (
                    [|resultVariable|]  ,
                    switchStatement     ,
                    resultVariable      )

            let lambda = 
                Expression.Lambda<Func<char, int, 'T>>(
                   body         ,
                   parameter0   ,
                   parameter1   )

            lambda.Compile ();
(*
        let fastChoice (parsers : (string*Parser<'T, 'UserState>) list) : Parser<'T, 'UserState> = 
            let choices = parsers |> List.map (fun (anyOf,v) -> (anyOf, Some v))
            let fm = fastMap choices None
            let ems = 
                parsers 
                |> List.collect (fun (anyOf,_) -> anyOf |> Seq.toList)
                |> List.map expectedChar
            fun ps -> 
                if ps.IsEndOfStream then failure ems
                else 
                    let ch  = ps.Peek ()
                    let f   = fm.Invoke(ch, 0)
                    match f with
                    | None      -> failure ems
                    | Some p    -> 
                        let me  = initialMerge ps ems
                        let r   = p ps
                        me.Merge ps r.ErrorMessages
                        if r.Ok then success r.Result me.Errors
                        else failure me.Errors
*)
        let hex2int c = 
            match c with
            | _ when c >= '0' && c <= '9'   -> (int c) - (int '0')     
            | _ when c >= 'a' && c <= 'f'   -> (int c) - (int 'a') + 10
            | _ when c >= 'A' && c <= 'F'   -> (int c) - (int 'A') + 10
            | _                             -> 0
        
        let makeDouble (d : int) (i : int64) (n :int) (f : float) (e : float) = 
            ((float d) * (pown 10. n) + (float i) + f)*e

        let p_ws            : Parser<unit, unit>        = spaces
        let p_token token   : Parser<unit, unit>        = skipChar token
        let p_wstoken token : Parser<unit, unit>        = attempt (p_ws >>. p_token token)

        let p_escape        : Parser<char, unit>      =  
                anyOf """"\/bfnrt"""
                |>> function
                    | 'b' -> '\b'
                    | 'f' -> '\f'
                    | 'n' -> '\n'
                    | 'r' -> '\r'
                    | 't' -> '\t'
                    | c   -> c
        let p_unicodeEscape : Parser<char, unit>      =
            p_token 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 -> 
                (hex2int h3)*0x1000 + (hex2int h2)*0x100 + (hex2int h1)*0x10 + hex2int h0 |> char
            )
        let p_char          : Parser<char, unit>        =
            choice
                [
                    noneOf """"\"""
                    p_token '\\' >>. (p_escape <|> p_unicodeEscape)
                ]
        let p_stringLiteral : Parser<string, unit>      =
            between (p_token '"') (p_token '"') (manyChars p_char)

        let p_digit1To9     : Parser<char, unit>        = anyOf "123456789"
        let p_digit         : Parser<int, unit>         = digit |>> hex2int
        let p_int           : Parser<int64*int, unit>   = many p_digit |>> (fun digits ->
                                let mutable result = 0L
                                for d in digits do
                                    result <- 10L * result + (int64 d)
                                result,digits.Length
                            )
        let p_e             : Parser<float, unit>       = 
            skipAnyOf "eE" >>. (choice [charReturn '-' 0.1;charReturn '+' 10.] <|> preturn 10.)
        let p_exponent      : Parser<float, unit>       = 
            p_e .>>. p_int |>> (fun (exp, (i,_)) -> pown exp (int i)) <|> preturn 1.
        let p_fraction      : Parser<float, unit>       = 
            (p_token '.' >>. (p_int |>> (fun (v,n) -> (float v) * (pown 0.1 n)))) <|> preturn 0.
        let p_sign          : Parser<float, unit>       = 
            (charReturn '-' -1.) <|> preturn 1.
        let p_digit19       : Parser<int, unit>         = 
            p_digit1To9 |>> hex2int
        let p_numberLiteral : Parser<float, unit>       = 
            p_sign .>>. choice 
                            [
                                // JSON doesn't allow numbers like 0123 (has to be 123).
                                // This is probably to avoid issues with octals numbers
                                pipe3 (p_token '0') p_fraction p_exponent (fun _ f e -> makeDouble 0 0L 0 f e)
                                pipe4 p_digit19 p_int p_fraction p_exponent (fun d (v,n) f e -> makeDouble d v n f e)
                            ] |>> (fun (s,n) -> s*n)


        let p_null          : Parser<JsonValue, unit>    = stringReturn "null"   JsonValue.Null
        let p_true          : Parser<JsonValue, unit>    = stringReturn "true"   <| JsonValue.Boolean true
        let p_false         : Parser<JsonValue, unit>    = stringReturn "false"  <| JsonValue.Boolean false
        let p_string        : Parser<JsonValue, unit>    = p_stringLiteral       |>> JsonValue.String
        let p_number        : Parser<JsonValue, unit>    = p_numberLiteral       |>> JsonValue.Float

        let rec p_value     : Parser<JsonValue, unit>    = 
            let p = 
                lazy
                    p_ws 
                    >>. choice
                        [
                            p_null 
                            p_true
                            p_false
                            p_string
                            p_number  
                            p_object
                            p_array  
                        ]
            fun ps -> p.Value ps
        and p_member        : Parser<string*JsonValue, unit> = 
            p_ws >>. p_stringLiteral .>> p_ws .>> (p_token ':') .>>. p_value
        and p_object        : Parser<JsonValue, unit>        = 
            between (p_token '{') (p_wstoken '}') (sepBy p_member (p_wstoken ',') |>> (List.toArray >> JsonValue.Record))
        and p_array         : Parser<JsonValue, unit>        = 
            between (p_token '[') (p_wstoken ']') (sepBy p_value (p_wstoken ',') |>> (List.toArray >> JsonValue.Array))

        let p_root          : Parser<JsonValue, unit>        = p_ws 
                                                                >>. choice 
                                                                    [
                                                                        p_object
                                                                        p_array  
                                                                    ]
    
        let p_json  = p_root .>> p_ws .>> eof
        let p_jsons = (many p_root) .>> p_ws .>> eof
    

    let Parse           str = run Details.p_json str
    let ParseMultiple   str = run Details.p_jsons str

open System
open System.Diagnostics
open System.IO
open FParsec
open JSONParser

let failures = ref 0

let failure (s : string) = 
    failures := !failures + 1
    printfn "FAILED: %s" s

let runTestCases () = 
    let testCases = 
        [
            // Simple cases
            """[]"""                , Some <| Array [||]
            """[null]"""            , Some <| Array [|Null|]
            """[true]"""            , Some <| Array [|Boolean true|]
            """[false]"""           , Some <| Array [|Boolean false|]
            """[""]"""              , Some <| Array [|String ""|]
            """["Test"]"""          , Some <| Array [|String "Test"|]
            """["Test\t"]"""        , Some <| Array [|String "Test\t"|]
            """["\"\\\//\b\f\n\r\t\u0041"]"""    
                                    , Some <| Array [|String "\"\\//\b\f\n\r\t\u0041"|]
            """[0]"""               , Some <| Array [|Float 0.|]
            """[0.5]"""             , Some <| Array [|Float 0.5|]
            """[1234]"""            , Some <| Array [|Float 1234.|]
            """[-1234]"""           , Some <| Array [|Float -1234.|]
            """[1234.25]"""         , Some <| Array [|Float 1234.25|]
            """[-1234.25]"""        , Some <| Array [|Float -1234.25|]
            """[1234.50E2]"""       , Some <| Array [|Float 123450.|]
            """[-1234.5E+2]"""      , Some <| Array [|Float -123450.|]
// TODO: Implement own comparer due to rounding issues
//            """[123450E-2]"""   , Some <| Array [Number 1234.50]
//            """[-123450e-2]"""  , Some <| Array [Number -1234.50]
            """[null,false]"""      , Some <| Array [|Null;Boolean false|]
            """[{}]"""              , Some <| Array [|Record [||]|]
            """{}"""                , Some <| Record [||]
            """{"a":null}"""        , Some <| Record [|"a",Null|]
            """{"a":[]}"""          , Some <| Record [|"a",Array [||]|]
            """{"a":[],"b":{}}"""   , Some <| Record [|"a",Array [||];"b",Record [||]|]
            // Whitespace cases
            """ []"""               , Some <| Array [||]
            """[] """               , Some <| Array [||]
            """ [] """              , Some <| Array [||]
            """[ true]"""           , Some <| Array [|Boolean true|]
            """[true ]"""           , Some <| Array [|Boolean true|]
            """[ true ]"""          , Some <| Array [|Boolean true|]
            """[null, true]"""      , Some <| Array [|Null;Boolean true|]
            """[null ,true]"""      , Some <| Array [|Null;Boolean true|]
            """[null , true]"""     , Some <| Array [|Null;Boolean true|]
            """ {}"""               , Some <| Record [||]
            """{} """               , Some <| Record [||]
            """ {} """              , Some <| Record [||]
            """{ "a":true}"""       , Some <| Record [|"a",Boolean true|]
            """{"a":true }"""       , Some <| Record [|"a",Boolean true|]
            """{ "a":true }"""      , Some <| Record [|"a",Boolean true|]
            """{"a" :true}"""       , Some <| Record [|"a",Boolean true|]
            """{"a": true}"""       , Some <| Record [|"a",Boolean true|]
            """{"a" : true}"""      , Some <| Record [|"a",Boolean true|]
            """{"a":[] ,"b":{}}"""  , Some <| Record [|"a",Array [||];"b",Record [||]|]
            """{"a":[], "b":{}}"""  , Some <| Record [|"a",Array [||];"b",Record [||]|]
            """{"a":[] , "b":{}}""" , Some <| Record [|"a",Array [||];"b",Record [||]|]
            // Failure cases
            """0"""             , None
            """true"""          , None
            """[,]"""           , None
            """[true,]"""       , None
            """[0123]"""        , None
            // Complex cases
        ]

    for (json, expected) in testCases do
        let result = Parse json

        match result, expected with
        |   (Failure _, None)                       -> ()
        |   (Success (r,_,_), Some e) when r = e    -> ()
        |   (Success (r,_,_), Some e)               -> 
            failure <| sprintf  "Parse was successful but didn't match expected:\nExpected:\n%A\nActual:\n%A\nJSON:\n%s" e r json
        |   (Success (r,_,_), None)                 -> 
            failure <| sprintf  "Parse was successful but expected to fail:Actual:\n%A\nJSON:\n%s" r json
        |   (Failure (d,_,_), Some e)                          -> 
            failure <| sprintf  "Parse failed:\nMessage:%s\nExpected:\n%A\nJSON:\n%s" d e json
        |   (Failure (d,_,_), Some e)                          -> 
            failure <| sprintf  "Parse failed: %s, %A, %s" d e json

let runPerformanceTestCases () =
    printfn "Running performance test cases"

    let samplePath      = AppDomain.CurrentDomain.BaseDirectory
    let documentPath    = Path.Combine (samplePath, "topics.json")

    try
        let document = File.ReadAllText documentPath

        // Dry run
        ignore <| ParseMultiple document

        let iterations  = 100

        printfn "Running %d iterations on document: %s using new parser" iterations documentPath
        let newStopWatch   = Stopwatch()
        newStopWatch.Start()
        for i in 1..iterations do
            ignore <| ParseMultiple document
        newStopWatch.Stop()

        printfn "Result: %d ms" newStopWatch.ElapsedMilliseconds

    with
        e -> 
            failure <| sprintf "Parsed failed for document: %s, message: %s" documentPath e.Message

[<EntryPoint>]
let main argv = 
    runTestCases ()
    runPerformanceTestCases ()

    0
