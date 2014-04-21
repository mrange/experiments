#if PARSER
module Parser =

    open OptimizedClosures
    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.Linq.Expressions
    open System.Text           

    let fastAnyOf (anyOf : string) (matchResult : bool) : Func<char, int, bool> =
        // For input string "Test" this generates the equivalent code to
        // Func<char, int> d = (ch, index) =>
        // {
        //    bool result;
        //    switch (ch)
        //    {
        //       case 'T':
        //       case 'e':
        //       case 's':
        //       case 't':
        //          result = matchResult;
        //          break;
        //       default:
        //          result = !matchResult;
        //          break;
        //    }
        //    return result;
        // }

        let parameter0     = Expression.Parameter   (typeof<char>   , "ch"      )
        let parameter1     = Expression.Parameter   (typeof<int>    , "index"   )
        let resultVariable = Expression.Variable    (typeof<bool>   , "result"  )

        let switchCase =
            Expression.SwitchCase    (
                Expression.Assign     (resultVariable, Expression.Constant matchResult)           ,
                anyOf |> Seq.map (fun ch -> Expression.Constant (ch) :> Expression) |> Seq.toArray)

        let switchStatement =
            Expression.Switch (
               parameter0                                                                           ,
               Expression.Assign        (resultVariable, Expression.Constant (not matchResult))     ,
               switchCase                                                                           )

        let body =
            Expression.Block (
                [|resultVariable|]  ,
                switchStatement     ,
                resultVariable      )

        let lambda =
            Expression.Lambda<Func<char, int, bool>>(
               body         ,
               parameter0   ,
               parameter1   )

        lambda.Compile ();

    // Generic parsers functions (similar look to FParsec)

    let initialCapacity = 16

    [<Struct>]
    type Substring(i : string, b : int, e : int) =
        member x.Input      = i
        member x.Begin      = b
        member x.End        = e
        member x.Length     = e - b
        member x.IsEmpty    = b = e

        member x.Str        = i.Substring(b, e - b)
        member x.Char idx   = i.[b + idx]

    let emptySubstring  = Substring("",0,0)
    let eosChar         = '\uFFFF'

    type CharTest = FSharpFunc<char,int, bool>

    type CharStream<'UserState>(input : string, userState : 'UserState) =

        let mutable position            = 0
        let mutable noErrorMessages     = true

        member x.Input                  = input
        member x.Position               = position
        member x.UserState              = userState
        member x.IsEndOfStream          = position >= input.Length || position < 0
        member x.NoErrorMessages        = noErrorMessages
        member x.Peek ()                = if x.IsEndOfStream then '\uFFFF' else input.[position]

        member x.SetPosition pos        = position <- pos
        member x.SetNoErrorMessages flag= noErrorMessages <- flag

        member x.Match atLeast atMost (test : CharTest) : Substring =
            Debug.Assert (atLeast >= 0)
            Debug.Assert (atMost >= atLeast)

            let i           = input
            let ``begin``   = position
            let length      = i.Length

            let remaining   = length - ``begin``

            if atLeast > remaining then
                emptySubstring
            else
                let required    = ``begin`` + atLeast
                let ``end``     = ``begin`` + min remaining atMost

                let mutable cont = true
                let mutable iter = 0
                let mutable pos  = ``begin``

                while cont && pos < ``end`` do
                    cont    <- test.Invoke(i.[pos], iter)
                    iter    <- iter + 1
                    pos     <- pos + 1

                let stopped = if cont then pos else pos - 1

                if required > stopped then
                    emptySubstring
                else
                    position <- stopped
                    Substring(i,``begin``, stopped)

        member x.MatchChar (test : CharTest) : char =
            let i           = input
            let pos         = position
            let length      = i.Length

            if pos < length && test.Invoke(i.[pos], 0) then
                position <- pos + 1
                i.[pos]
            else
                eosChar

        member x.SkipWhitespaces () : unit =
            let i           = input
            let length      = i.Length

            let mutable pos  = position

            while   pos < length &&
                    match i.[pos] with
                    | ' '
                    | '\t'
                    | '\n'
                    | '\r'  -> true
                    | _     -> false
                do
                pos <- pos + 1

            position <- pos

    type ErrorMessage =
        | Expected      of string
        | NotExpected   of string

    let noErrors : ErrorMessage list = []

    let expectedChar    (ch : char) = Expected      <| "'" + ch.ToString() + "'"
    let notExpectedChar (ch : char) = NotExpected   <| "'" + ch.ToString() + "'"

    type MergedErrors() =
        let mutable pos     = -1
        let mutable errors  = []

        member x.Merge (ps : CharStream<'Result>) (newErrors : ErrorMessage list) =
            if ps.NoErrorMessages then
                ()
            else
                let newPos = ps.Position
                if newPos <> pos then
                    pos <- newPos
                    errors <- newErrors
                else
                    errors <- newErrors@errors
        member x.Errors = errors


    let inline initialMerge (ps : CharStream<'Result>) (r : ErrorMessage list) : MergedErrors =
        let me = MergedErrors()
        me.Merge ps r
        me

    let inline emptyMerge (ps : CharStream<'Result>) : MergedErrors =
        initialMerge ps []

    [<Struct>]
    type Reply<'Result>(isOk : bool, result : 'Result, errorMessages : ErrorMessage list) =

        member x.Ok                 = isOk
        member x.Error              = not isOk
        member x.Result             = result
        member x.ErrorMessages      = errorMessages

    type Parser<'Result, 'UserState> = CharStream<'UserState> -> Reply<'Result>

    type ParserResult<'Result, 'UserState> =
        | Success           of 'Result * 'UserState * int
        | Failure           of string * int * 'UserState

    let prettify (ems : ErrorMessage list) (ps : CharStream<'UserState>) =
        let expected    = HashSet<string>()
        let notExpected = HashSet<string>()
        let rec collectMessages (ems : ErrorMessage) =
            match ems with
            | Expected      m   ->  ignore <| expected.Add m
            | NotExpected   m   ->  ignore <| notExpected.Add m

        for em in ems do
            collectMessages em

        let input       =   System.String
                                (
                                    ps.Input
                                    |> Seq.map (fun ch -> if Char.IsWhiteSpace ch then ' ' else ch)
                                    |> Seq.toArray
                                )
        let pos         = ps.Position
        let snippetSize = 80
        let midSnippet  = snippetSize / 2
        let beginSnippet= max 0 <| pos - midSnippet
        let endSnippet  = min input.Length <| pos + midSnippet
        let snippet     = input.Substring(beginSnippet, endSnippet - beginSnippet)
        let indicator   = System.String ('-', pos - beginSnippet)

        let e   = System.String.Join(", ", expected     |> Seq.sort |> Seq.toArray)
        let ne  = System.String.Join(", ", notExpected  |> Seq.sort |> Seq.toArray)
        let reason =
            match e, ne with
            | "","" -> ""
            | l,""  -> "expecting " + l
            | "",r  -> "didn't expect " + r
            | l,r   -> "expected " + l + " and didn't expect " +  r

        sprintf
            "Parse error at position: %d, input:\n%s\n%s^\nFound '%s' but %s"
            pos
            snippet
            indicator
            (input.[pos].ToString())
            reason

    let run (p : Parser<'T, unit>) (s : string) : ParserResult<'T, unit> =
        let ps = CharStream(s, ())
        let r = p ps
        if r.Ok then Success (r.Result, ps.UserState, ps.Position)
        else
            // Failed, now generate error message
            ps.SetPosition 0
            ps.SetNoErrorMessages false
            let r = p ps
            Failure (prettify r.ErrorMessages ps, ps.Position, ps.UserState)

    let inline success (v : 'T) ems    = Reply<'T> (true, v, ems)
    let inline failure ems             = Reply<'T> (false, Unchecked.defaultof<'T>, ems)

    let preturn (v : 'T) : Parser<'T, 'UserState> = fun ps -> success v []

    let debug (p : Parser<'T, 'UserState>) : Parser<'T, 'UserState> =
        fun ps ->
            let r = p ps
            r

    let eof : Parser<unit, 'UserState> =
        let ems = [Expected "EOF"]
        fun ps ->
            match ps.IsEndOfStream with
            | false -> failure ems
            | true  -> success () ems

    let spaces : Parser<unit, 'UserState> =
        fun ps ->
            ps.SkipWhitespaces ()
            success () noErrors

    let skipChar (c : char): Parser<unit, 'UserState> =
        let test = CharTest.Adapt <| fun ch _ -> ch = c
        let ems = [expectedChar c]
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success () ems
            else failure ems

    let attempt (p : Parser<'T, 'UserState>) : Parser<'T, 'UserState> =
        fun ps ->
            let pos = ps.Position
            let r = p ps
            if r.Error then ps.SetPosition pos
            r

    let orElse (l : Parser<'T, 'UserState>) (r : Parser<'T, 'UserState>) : Parser<'T, 'UserState> =
        fun ps ->
            let rl = l ps
            let me = initialMerge ps rl.ErrorMessages
            if rl.Ok then rl
            else
                let rr = r ps
                me.Merge ps rr.ErrorMessages
                if rr.Ok then success rr.Result me.Errors
                else failure me.Errors
    let ( <|> ) = orElse

    let map (p : Parser<'TFrom, 'UserState>) (m : 'TFrom->'TTo) : Parser<'TTo, 'UserState> =
        fun ps ->
            let r = p ps
            if r.Ok then success (m r.Result) r.ErrorMessages
            else failure r.ErrorMessages
    let ( |>> ) = map

    let combine (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'L*'R, 'UserState> =
        fun ps ->
            let rl = l ps
            let me = initialMerge ps rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let rr = r ps
                me.Merge ps rr.ErrorMessages
                if rr.Error then failure me.Errors
                else success (rl.Result, rr.Result) me.Errors
    let ( .>>. ) = combine

    let keepLeft (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'L, 'UserState> =
        fun ps ->
            let rl = l ps
            let me = initialMerge ps rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let rr = r ps
                me.Merge ps rr.ErrorMessages
                if rr.Error then failure me.Errors
                else success rl.Result me.Errors
    let ( .>> ) = keepLeft

    let keepRight (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'R, 'UserState> =
        fun ps ->
            let rl = l ps
            let me = initialMerge ps rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let rr = r ps
                me.Merge ps rr.ErrorMessages
                if rr.Error then failure me.Errors
                else success rr.Result me.Errors
    let ( >>. ) = keepRight

    let skipSatisfyImpl (test : CharTest) (ems : ErrorMessage list) : Parser<unit, 'UserState> =
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success () ems
            else failure ems

    let satisfyImpl (test : CharTest) (ems : ErrorMessage list) : Parser<char, 'UserState> =
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success ch ems
            else failure ems

    let skipAnyOf (s : string) : Parser<unit, 'UserState> =
        let fastSet     = fastAnyOf s true
        let test        = CharTest.Adapt <| fun ch p -> fastSet.Invoke(ch,p)
        let ems         = s |> Seq.map expectedChar |> Seq.toList
        skipSatisfyImpl test ems

    let anyOf (s : string) : Parser<char, 'UserState> =
        let fastSet     = fastAnyOf s true
        let test        = CharTest.Adapt <| fun ch p -> fastSet.Invoke(ch,p)
        let ems         = s |> Seq.map expectedChar |> Seq.toList
        satisfyImpl test ems

    let noneOf (s : string) : Parser<char, 'UserState> =
        let fastSet     = fastAnyOf s false
        let test        = CharTest.Adapt <| fun ch p -> fastSet.Invoke(ch,p)
        let ems         = s |> Seq.map notExpectedChar |> Seq.toList
        satisfyImpl test ems

    let digit : Parser<char, 'UserState> =
        let test    = CharTest.Adapt <| fun ch _ ->
            match ch with
            | _ when ch >= '0' && ch <= '9' -> true
            | _ -> false
        let ems     = [Expected "Digit"]
        fun ps -> satisfyImpl test ems ps

    let hex : Parser<char, 'UserState> =
        let test    = CharTest.Adapt <| fun ch _ ->
            match ch with
            | _ when ch >= '0' && ch <= '9' -> true
            | _ when ch >= 'a' && ch <= 'f' -> true
            | _ when ch >= 'A' && ch <= 'F' -> true
            | _ -> false
        let ems = [Expected "HexDigit"]
        fun ps -> satisfyImpl test ems ps

    let pipe2
            (p0 : Parser<'T0, 'UserState>)
            (p1 : Parser<'T1, 'UserState>)
            (m  : 'T0->'T1->'T)
            : Parser<'T, 'UserState> =
        let fm = FSharpFunc<_,_,_>.Adapt m
        fun ps ->
            let r0 = p0 ps
            let me = initialMerge ps r0.ErrorMessages
            if r0.Error then failure me.Errors
            else
                let r1 = p1 ps
                me.Merge ps r1.ErrorMessages
                if r1.Error then failure me.Errors
                else
                    success (fm.Invoke(r0.Result, r1.Result)) me.Errors

    let pipe3
            (p0 : Parser<'T0, 'UserState>)
            (p1 : Parser<'T1, 'UserState>)
            (p2 : Parser<'T2, 'UserState>)
            (m  : 'T0->'T1->'T2->'T)
            : Parser<'T, 'UserState> =
        let fm = FSharpFunc<_,_,_,_>.Adapt m
        fun ps ->
            let r0 = p0 ps
            let me = initialMerge ps r0.ErrorMessages
            if r0.Error then failure me.Errors
            else
                let r1 = p1 ps
                me.Merge ps r1.ErrorMessages
                if r1.Error then failure me.Errors
                else
                    let r2 = p2 ps
                    me.Merge ps r2.ErrorMessages
                    if r2.Error then failure me.Errors
                    else
                        success (fm.Invoke(r0.Result, r1.Result, r2.Result)) me.Errors

    let pipe4
            (p0 : Parser<'T0, 'UserState>)
            (p1 : Parser<'T1, 'UserState>)
            (p2 : Parser<'T2, 'UserState>)
            (p3 : Parser<'T3, 'UserState>)
            (m  : 'T0->'T1->'T2->'T3->'T)
            : Parser<'T, 'UserState> =
        let fm = FSharpFunc<_,_,_,_,_>.Adapt m
        fun ps ->
            let r0 = p0 ps
            let me = initialMerge ps r0.ErrorMessages
            if r0.Error then failure me.Errors
            else
                let r1 = p1 ps
                me.Merge ps r1.ErrorMessages
                if r1.Error then failure me.Errors
                else
                    let r2 = p2 ps
                    me.Merge ps r2.ErrorMessages
                    if r2.Error then failure me.Errors
                    else
                        let r3 = p3 ps
                        me.Merge ps r3.ErrorMessages
                        if r3.Error then failure me.Errors
                        else
                            success (fm.Invoke(r0.Result, r1.Result, r2.Result, r3.Result)) me.Errors

    let choice (parsers : Parser<'T, 'UserState> list) : Parser<'T, 'UserState> =
        fun ps ->
            let me = emptyMerge ps
            let mutable result      = None
            let mutable remaining   = parsers
            while result.IsNone && remaining.Length > 0 do
                let p = remaining.Head
                remaining <- remaining.Tail
                let r = p ps
                me.Merge ps r.ErrorMessages
                if r.Ok then result <- Some r.Result

            match result with
            | None          -> failure me.Errors
            | Some v        -> success v me.Errors

    let between
            (b : Parser<_, 'UserState>)
            (e : Parser<_, 'UserState>)
            (p : Parser<'T, 'UserState>)
            : Parser<'T, 'UserState> =
            pipe3 b p e <| fun _ v _ -> v

    let charReturn (c : char) (v : 'T) : Parser<'T, 'UserState> =
        let ems     = [expectedChar c]
        let test    = CharTest.Adapt <| fun ch _ -> ch = c
        fun ps ->
            let ch = ps.MatchChar test
            if ch <> eosChar then success v ems
            else failure ems

    let stringReturn (s : string) (v : 'T) : Parser<'T, 'UserState> =
        let length      = s.Length
        let ems         = [Expected <| "'" + s + "'"]
        let test    = CharTest.Adapt <| fun ch p -> s.[p] = ch
        fun ps ->
            let ss = ps.Match length length test
            if not ss.IsEmpty then success v ems
            else failure ems

    let many (p : Parser<'T, 'UserState>) : Parser<'T list, 'UserState> =
        fun ps ->
            let me      = emptyMerge ps
            let result  = List<'T>(initialCapacity)
            while
                let r = p ps in // TODO: Why is this in required?
                let _ = me.Merge ps r.ErrorMessages in
                if r.Error then false
                else
                    result.Add r.Result
                    true
                do
                ()

            success (result |> Seq.toList) me.Errors

    let manyChars (p : Parser<char, 'UserState>) : Parser<string, 'UserState> =
        fun ps ->
            let me      = emptyMerge ps
            let result  = StringBuilder()
            while
                let r = p ps in // TODO: Why is this in required?
                let _ = me.Merge ps r.ErrorMessages in
                if r.Error then false
                else
                    ignore <| result.Append r.Result
                    true
                do
                ()

            success (result.ToString()) me.Errors

    let sepBy (p : Parser<'T, 'UserState>) (sep : Parser<_, 'UserState>) : Parser<'T list, 'UserState> =
        fun ps ->
            let result  = List<'T>(initialCapacity)

            let ri  = p ps
            let me  = initialMerge ps ri.ErrorMessages

            if ri.Error then success [] me.Errors
            else
                let mutable failed = false

                result.Add ri.Result
                while
                    let rs = sep ps in  // TODO: Why is this in required?
                    let _ = me.Merge ps rs.ErrorMessages in
                    if rs.Error then false
                    else
                        let r = p ps
                        me.Merge ps r.ErrorMessages
                        if r.Error then
                            failed <- true
                            false
                        else
                            result.Add r.Result
                            true
                    do
                    ()
                if failed then failure me.Errors
                else
                    success (result |> Seq.toList) me.Errors

    let createParserForwardedToRef () =
        let dummyParser = fun stream -> failwith "a parser created with createParserForwardedToRef was not initialized"
        let r = ref dummyParser
        (fun stream -> !r stream), r : Parser<_,'u> * Parser<_,'u> ref

open Parser
#else
open FParsec
#endif

module JSONParser =

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

(*
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

        let (p_value : Parser<JsonValue, unit>, p_value_ref) = createParserForwardedToRef ()

        let p_member        : Parser<string*JsonValue, unit> =
            p_ws >>. p_stringLiteral .>> p_ws .>> (p_token ':') .>>. p_value
        let p_object        : Parser<JsonValue, unit>        =
            between (p_token '{') (p_wstoken '}') (sepBy p_member (p_wstoken ',') |>> (List.toArray >> JsonValue.Record))
        let p_array         : Parser<JsonValue, unit>        =
            between (p_token '[') (p_wstoken ']') (sepBy p_value (p_wstoken ',') |>> (List.toArray >> JsonValue.Array))

        let p_root          : Parser<JsonValue, unit>        = p_ws
                                                                >>. choice
                                                                    [
                                                                        p_object
                                                                        p_array
                                                                    ]

        p_value_ref := 
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

        let p_json  = p_root .>> p_ws .>> eof
        let p_jsons = (many p_root) .>> p_ws .>> eof


    let Parse           str = run Details.p_json str
    let ParseMultiple   str = run Details.p_jsons str

open JSONParser
open System.Diagnostics
open System.IO
open System.Text

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
        |   (Failure (_,_,_), None)                       -> ()
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

    let samplePath      = System.AppDomain.CurrentDomain.BaseDirectory
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

let buildString (ss : seq<string>) =
    let sb = StringBuilder()
    for s in ss do
        ignore <| sb.Append s
    sb.ToString ()

let runPerformanceRun () =
    printfn "Running performance run"

    let random          = System.Random(19740531)                            

    let document        = [ for i in 0..100 -> "{}" ] |> buildString

    let p_root          = many Details.p_root
    let parse   str     = run p_root str

    // Dry run
    let r = parse document

    let iterations  = 400

    printfn "Running %d iterations on document using new parser" iterations
    let newStopWatch   = Stopwatch()
    newStopWatch.Start()
    for i in 1..iterations do
        ignore <| parse document
    newStopWatch.Stop()

    printfn "Result: %d ms" newStopWatch.ElapsedMilliseconds

[<EntryPoint>]
let main argv =
    runTestCases ()
    runPerformanceTestCases ()
//    runPerformanceRun ()
    0
