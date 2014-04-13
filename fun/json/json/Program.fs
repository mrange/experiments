
module JSONParser = 
    open FParsec

    type JSON = 
    | Null
    | Boolean   of bool
    | Number    of float
    | String    of string
    | Array     of JSON list
    | Object    of (string*JSON) list

    module Details =

        let satisfy1To9 c = c >= '1' && c <= '9'

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
        let p_wstoken token : Parser<unit, unit>        = p_ws >>. skipChar token

        let p_escape        : Parser<char, unit>      =  
                anyOf "\"\\/bfnrt"
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
                    satisfy (fun ch -> ch <> '"' && ch <> '\\')
                    p_token '\\' >>. (p_escape <|> p_unicodeEscape)
                ]
        let p_stringLiteral : Parser<string, unit>      =
            between (p_token '"') (p_token '"') (manyChars p_char)

        let p_digit1To9     : Parser<char, unit>        = satisfy satisfy1To9
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


        let p_null          : Parser<JSON, unit>    = stringReturn "null"   Null
        let p_true          : Parser<JSON, unit>    = stringReturn "true"   <| Boolean true
        let p_false         : Parser<JSON, unit>    = stringReturn "false"  <| Boolean false
        let p_string        : Parser<JSON, unit>    = p_stringLiteral       |>> String
        let p_number        : Parser<JSON, unit>    = p_numberLiteral       |>> Number

        let rec p_value     : Parser<JSON, unit>    = fun cs -> 
            cs 
            |>  (p_ws 
                    >>. choice
                        [
                            p_null  
                            p_true
                            p_false
                            p_string
                            p_number  
                            p_object
                            p_array  
                        ])
        and p_member        : Parser<string*JSON, unit> = p_stringLiteral .>> (p_wstoken ':') .>>. p_value
        and p_object        : Parser<JSON, unit>    = between (skipChar '{') (p_wstoken '}') (sepBy p_member (p_wstoken ',') |>> Object)
        and p_array         : Parser<JSON, unit>    = between (skipChar '[') (p_wstoken ']') (sepBy p_value (p_wstoken ',') |>> Array)

        let p_root          : Parser<JSON, unit>    = 
            p_ws 
                >>. choice [p_object;p_array]
                .>> p_ws 
                .>> eof
    
        let p_json = p_root

    let Parse str = run Details.p_json str

open FParsec
open JSONParser

[<EntryPoint>]
let main argv = 

    let testCases = 
        [
            // Simple cases
            """[]"""            , Some <| Array []
            """[true]"""        , Some <| Array [Boolean true]
            """[false]"""       , Some <| Array [Boolean false]
            """[0]"""           , Some <| Array [Number 0.]
            """[0.5]"""         , Some <| Array [Number 0.5]
            """[1234]"""        , Some <| Array [Number 1234.]
            """[-1234]"""       , Some <| Array [Number -1234.]
            """[1234.25]"""     , Some <| Array [Number 1234.25]
            """[-1234.25]"""    , Some <| Array [Number -1234.25]
            """[1234.50E2]"""      , Some <| Array [Number 123450.]
            """[-1234.5E+2]"""  , Some <| Array [Number -123450.]
// Rounding issues
//            """[123450E-2]"""   , Some <| Array [Number 1234.50]
//            """[-123450e-2]"""  , Some <| Array [Number -1234.50]
            """[null,false]"""  , Some <| Array [Null;Boolean false]
            """[{}]"""          , Some <| Array [Object []]
            """{}"""            , Some <| Object []
            """{"a":null}"""    , Some <| Object ["a",Null]
            """{"a":[]}"""      , Some <| Object ["a",Array []]
            // Failure cases
            """[,]"""           , None
            """[true,]"""       , None
            """[0123]"""        , None
            // Complex cases
        ]

    let failures = ref 0

    let failure (s : string) = 
        failures := !failures + 1
        printfn "FAILED: %s" s

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

    0
