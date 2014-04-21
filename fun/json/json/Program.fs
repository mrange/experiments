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

        let mutable position                = 0
        let mutable noErrorMessages         = true

        member x.Input                      = input
        member x.Position                   = position
        member x.UserState                  = userState
        member x.IsEndOfStream              = position >= input.Length || position < 0
        member x.NoErrorMessages            = noErrorMessages
        member x.Peek ()                    = if x.IsEndOfStream then '\uFFFF' else input.[position]

        member x.SetPosition pos            = position <- pos
        member x.SetNoErrorMessages flag    = noErrorMessages <- flag

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

        member x.SkipWhitespaces : unit =
            let i           = input
            let length      = i.Length

            let mutable cont = true
            let mutable pos  = position

            while cont && pos < length do
                cont    <-  match i.[pos] with
                            | ' '   
                            | '\t'  
                            | '\n'  
                            | '\r'  -> true
                            | _     -> false
                pos     <- pos + 1

            let stopped = if cont then pos else pos - 1

            position <- stopped

    type ErrorMessage =
        | Expected      of string
        | NotExpected   of string

    let noErrors : ErrorMessage list = []

    let expectedChar    (ch : char) = Expected      <| "'" + ch.ToString() + "'"
    let notExpectedChar (ch : char) = NotExpected   <| "'" + ch.ToString() + "'"

    type MergedErrors = int*ErrorMessage list

    let inline getErrors (me : MergedErrors) : ErrorMessage list =
        let _, errors = me 
        errors

    let inline mergeErrors (me : MergedErrors) (ps : CharStream<'Result>) (newErrors : ErrorMessage list) : MergedErrors = 
        let newPos      = ps.Position
        let pos, errors = me 
        if ps.NoErrorMessages then
           me
        elif newPos <> pos then
            newPos,newErrors
        else
            newPos,(newErrors@errors)

    let inline initialMerge (ps : CharStream<'Result>) (r : ErrorMessage list) : MergedErrors =
        ps.Position,r

    let inline emptyMerge (ps : CharStream<'Result>) : MergedErrors =
        ps.Position,noErrors

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
            ignore <| ps.SkipWhitespaces
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
            let mutable me = initialMerge ps rl.ErrorMessages
            if rl.Ok then rl
            else
                let rr = r ps
                me <- mergeErrors me ps rr.ErrorMessages
                if rr.Ok then success rr.Result <| getErrors me
                else failure <| getErrors me
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
            let mutable me = initialMerge ps rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let rr = r ps
                me <- mergeErrors me ps rr.ErrorMessages
                if rr.Error then failure <| getErrors me
                else success (rl.Result, rr.Result) <| getErrors me
    let ( .>>. ) = combine

    let keepLeft (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'L, 'UserState> =
        fun ps ->
            let rl = l ps
            let mutable me = initialMerge ps rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let rr = r ps
                me <- mergeErrors me ps rr.ErrorMessages
                if rr.Error then failure <| getErrors me
                else success rl.Result <| getErrors me
    let ( .>> ) = keepLeft

    let keepRight (l : Parser<'L, 'UserState>) (r : Parser<'R, 'UserState>) : Parser<'R, 'UserState> =
        fun ps ->
            let rl = l ps
            let mutable me = initialMerge ps rl.ErrorMessages
            if rl.Error then failure rl.ErrorMessages
            else
                let rr = r ps
                me <- mergeErrors me ps rr.ErrorMessages
                if rr.Error then failure <| getErrors me
                else success rr.Result <| getErrors me
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
            let mutable me = initialMerge ps r0.ErrorMessages
            if r0.Error then failure <| getErrors me
            else
                let r1 = p1 ps
                me <- mergeErrors me ps r1.ErrorMessages
                if r1.Error then failure <| getErrors me
                else
                    success (fm.Invoke(r0.Result, r1.Result)) <| getErrors me

    let pipe3
            (p0 : Parser<'T0, 'UserState>)
            (p1 : Parser<'T1, 'UserState>)
            (p2 : Parser<'T2, 'UserState>)
            (m  : 'T0->'T1->'T2->'T)
            : Parser<'T, 'UserState> =
        let fm = FSharpFunc<_,_,_,_>.Adapt m
        fun ps ->
            let r0 = p0 ps
            let mutable me = initialMerge ps r0.ErrorMessages
            if r0.Error then failure <| getErrors me
            else
                let r1 = p1 ps
                me <- mergeErrors me ps r1.ErrorMessages
                if r1.Error then failure <| getErrors me
                else
                    let r2 = p2 ps
                    me <- mergeErrors me ps r2.ErrorMessages
                    if r2.Error then failure <| getErrors me
                    else
                        success (fm.Invoke(r0.Result, r1.Result, r2.Result)) <| getErrors me

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
            let mutable me = initialMerge ps r0.ErrorMessages
            if r0.Error then failure <| getErrors me
            else
                let r1 = p1 ps
                me <- mergeErrors me ps r1.ErrorMessages
                if r1.Error then failure <| getErrors me
                else
                    let r2 = p2 ps
                    me <- mergeErrors me ps r2.ErrorMessages
                    if r2.Error then failure <| getErrors me
                    else
                        let r3 = p3 ps
                        me <- mergeErrors me ps r3.ErrorMessages
                        if r3.Error then failure <| getErrors me
                        else
                            success (fm.Invoke(r0.Result, r1.Result, r2.Result, r3.Result)) <| getErrors me

    let choice (parsers : Parser<'T, 'UserState> list) : Parser<'T, 'UserState> =
        fun ps ->
            let mutable me = emptyMerge ps
            let mutable result      = None
            let mutable remaining   = parsers
            while result.IsNone && remaining.Length > 0 do
                let p = remaining.Head
                remaining <- remaining.Tail
                let r = p ps
                me <- mergeErrors me ps r.ErrorMessages
                if r.Ok then result <- Some r.Result

            match result with
            | None          -> failure <| getErrors me
            | Some v        -> success v <| getErrors me

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
            let mutable me  = emptyMerge ps
            let result      = List<'T>(initialCapacity)
            while
                let r = p ps in // TODO: Why is this in required?
                let _ = me <- mergeErrors me ps r.ErrorMessages in
                if r.Error then false
                else
                    result.Add r.Result
                    true
                do
                ()

            success (result |> Seq.toList) <| getErrors me

    let manyChars (p : Parser<char, 'UserState>) : Parser<string, 'UserState> =
        fun ps ->
            let mutable me  = emptyMerge ps
            let result      = StringBuilder()
            while
                let r = p ps in // TODO: Why is this in required?
                let _ = me <- mergeErrors me ps r.ErrorMessages in
                if r.Error then false
                else
                    ignore <| result.Append r.Result
                    true
                do
                ()

            success (result.ToString()) <| getErrors me

    let sepBy (p : Parser<'T, 'UserState>) (sep : Parser<_, 'UserState>) : Parser<'T list, 'UserState> =
        fun ps ->
            let result  = List<'T>(initialCapacity)

            let ri          = p ps
            let mutable me  = initialMerge ps ri.ErrorMessages

            if ri.Error then success [] <| getErrors me
            else
                let mutable failed = false

                result.Add ri.Result
                while
                    let rs = sep ps in  // TODO: Why is this in required?
                    let _ = me <- mergeErrors me ps ri.ErrorMessages in
                    if rs.Error then false
                    else
                        let r = p ps
                        me <- mergeErrors me ps r.ErrorMessages
                        if r.Error then
                            failed <- true
                            false
                        else
                            result.Add r.Result
                            true
                    do
                    ()
                if failed then failure <| getErrors me
                else
                    success (result |> Seq.toList) <| getErrors me

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
    let document        = """[{"photo":"https://s3.amazonaws.com/coursera/topics/ml/large-icon.png","preview_link":"https://class.coursera.org/ml/lecture/preview","small_icon_hover":"https://s3.amazonaws.com/coursera/topics/ml/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/ml/large-icon.png","video":"e0WKJLovaZg","university-ids":["stanford"],"id":2,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"","china_mirror":2,"favicon":"https://coursera-university-assets.s3.amazonaws.com/dc/581cda352d067023dcdcc0d9efd36e/favicon-stanford.ico","website_facebook":"","logo":"https://coursera-university-assets.s3.amazonaws.com/d8/4c69670e0826e42c6cd80b4a02b9a2/stanford.png","background_color":"","id":1,"location_city":"Palo Alto","location_country":"US","location_lat":37.44188340000000000,"location":"Palo Alto, CA, United States","primary_color":"#8c1515","abbr_name":"Stanford","website":"","description":"The Leland Stanford Junior University, commonly referred to as Stanford University or Stanford, is an American private research university located in Stanford, California on an 8,180-acre (3,310 ha) campus near Palo Alto, California, United States.","short_name":"stanford","landing_page_banner":"","mailing_list_id":null,"website_youtube":"","partner_type":1,"banner":"","location_state":"CA","name":"Stanford University","square_logo":"","square_logo_source":"","square_logo_svg":"","location_lng":-122.14301949999998000,"home_link":"http://online.stanford.edu/","class_logo":"https://coursera-university-assets.s3.amazonaws.com/21/9a0294e2bf773901afbfcb5ef47d97/Stanford_Coursera-200x48_RedText_BG.png","display":true}],"self_service_course_id":null,"short_description":"Learn about the most effective machine learning techniques, and gain practice implementing them and getting them to work for yourself.","short_name":"ml","category-ids":["stats","cs-ai"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/ml/small-icon.hover.png","instructor":"Andrew Ng, Associate Professor","categories":[{"id":16,"name":"Statistics and Data Analysis","mailing_list_id":null,"short_name":"stats","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":17,"name":"Computer Science: Artificial Intelligence","mailing_list_id":null,"short_name":"cs-ai","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Machine Learning","language":"en","courses":[{"grading_policy_distinction":"N/A","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":20,"duration_string":"10 weeks","signature_track_last_chance_time":null,"id":152,"start_month":8,"certificate_description":"Congratulations! You have successfully completed the online Machine Learning course (ml-class.org). To successfully complete the course, students were required to watch lectures, review questions and complete programming assignments. ","start_date_string":"20 August 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2013-03-10","certificates_ready":true,"signature_track_price":null,"statement_design_id":8,"signature_track_registration_open":false,"topic_id":2,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":100.0,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":1,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":100.0,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":null,"end_date":null,"notified_subscribers":true,"instructors":[232841],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-002","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on 18 equally-weighted quizzes (1/3 of the total grade) and 8 programming assignments (2/3 of the total grade).  The lowest two quiz grades were dropped, as well as the lowest programming assignment grade. To receive a Statement of Accomplishment, you must have obtained 80% or more of the maximum possible score.","ace_open_date":null,"home_link":"https://class.coursera.org/ml-2012-002/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[152],"display":true},{"photo":"https://s3.amazonaws.com/coursera/topics/pgm/large-icon.png","preview_link":"https://class.coursera.org/pgm/lecture/preview","small_icon_hover":"https://s3.amazonaws.com/coursera/topics/pgm/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/pgm/large-icon.png","video":"bVMBe50GfnI","university-ids":["stanford"],"id":3,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"","china_mirror":2,"favicon":"https://coursera-university-assets.s3.amazonaws.com/dc/581cda352d067023dcdcc0d9efd36e/favicon-stanford.ico","website_facebook":"","logo":"https://coursera-university-assets.s3.amazonaws.com/d8/4c69670e0826e42c6cd80b4a02b9a2/stanford.png","background_color":"","id":1,"location_city":"Palo Alto","location_country":"US","location_lat":37.44188340000000000,"location":"Palo Alto, CA, United States","primary_color":"#8c1515","abbr_name":"Stanford","website":"","description":"The Leland Stanford Junior University, commonly referred to as Stanford University or Stanford, is an American private research university located in Stanford, California on an 8,180-acre (3,310 ha) campus near Palo Alto, California, United States.","short_name":"stanford","landing_page_banner":"","mailing_list_id":null,"website_youtube":"","partner_type":1,"banner":"","location_state":"CA","name":"Stanford University","square_logo":"","square_logo_source":"","square_logo_svg":"","location_lng":-122.14301949999998000,"home_link":"http://online.stanford.edu/","class_logo":"https://coursera-university-assets.s3.amazonaws.com/21/9a0294e2bf773901afbfcb5ef47d97/Stanford_Coursera-200x48_RedText_BG.png","display":true}],"self_service_course_id":null,"short_description":"In this class, you will learn the basics of the PGM representation and how to construct them, using both human knowledge and machine learning techniques.","short_name":"pgm","category-ids":["cs-ai"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/pgm/small-icon.hover.png","instructor":"Daphne Koller, Professor","categories":[{"id":17,"name":"Computer Science: Artificial Intelligence","mailing_list_id":null,"short_name":"cs-ai","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Probabilistic Graphical Models","language":"en","courses":[{"grading_policy_distinction":"The final grade was based on 20 assessments (25% of the final grade), 9 programming assignments with companion quizzes (together worth 63% of the final grade) and a final exam (12% of the final grade). All assessments within a given category are weighted equally. To receive a Statement of Accomplishment, you must have obtained 70% or more of the maximum possible score.","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":24,"duration_string":"11 weeks","signature_track_last_chance_time":null,"id":82,"start_month":9,"certificate_description":"This graduate-level course covers the essentials of probabilistic graphical models and their applications: the representation of Bayesian and Markov networks; exact and approximate inference in these networks; and parameter and structure learning.","start_date_string":"24 September 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2012-12-19","certificates_ready":true,"signature_track_price":null,"statement_design_id":8,"signature_track_registration_open":false,"topic_id":3,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":84.5634194535585,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":2,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":81.3340565033494,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[1257],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-002","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on 20 assignments (77.5% of the final grade) and a final exam (22.5% of the final grade). All assessments within a given category are weighted equally. To receive a Statement of Accomplishment, you must have achieved 70% or more of the maximum possible score.","ace_open_date":null,"home_link":"https://class.coursera.org/pgm-2012-002/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[82],"display":true},{"photo":"https://s3.amazonaws.com/coursera/topics/modelthinking/large-icon.png","preview_link":"https://class.coursera.org/modelthinking-004/lecture/preview","small_icon_hover":"https://s3.amazonaws.com/coursera/topics/modelthinking/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/modelthinking/large-icon.png","video":"RX5LBZvEh10","university-ids":["umich"],"id":11,"universities":[{"rectangular_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/75/07a2c2f614597144b29c9e63bbee75/U-M_Coursera_Logo-2.svg","wordmark":null,"website_twitter":"umich","china_mirror":3,"favicon":"https://coursera-university-assets.s3.amazonaws.com/50/52265d555e7bf0b0b86f7a1a07a183/favicon-umich.ico","website_facebook":"universityofmichigan","logo":"https://coursera-university-assets.s3.amazonaws.com/99/263203c94842c44bcf757c4a801e8f/U-M_LogoSmHoriz_160x60.png","background_color":"","id":3,"location_city":"Ann Arbor","location_country":"US","location_lat":42.28082560000000000,"location":"Ann Arbor, MI","primary_color":"#002e5f","abbr_name":"Michigan","website":"http://www.umich.edu/","description":"The University of Michigan is a public research university located in Ann Arbor, Michigan in the United States.","short_name":"umich","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/03/70c6a457583d267a869cb6555b4de5/DIL-17Apr12_AT150a_EDIT_TO_2.jpg","mailing_list_id":null,"website_youtube":"user/um","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/36/1d953d8380a15d8eb0d307c7cde0bb/banner-umich.jpg","location_state":"MI","name":"University of Michigan","square_logo":"https://coursera-university-assets.s3.amazonaws.com/70/de505d47be7d3a063b51b6f856a6e2/New-Block-M-Stacked-Blue-295C_600x600.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/26/23ecbedd1043250eab7d1b0a41696b/New-Block-M-Stacked-Blue-295C_600x600.png","square_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/6b/fce84d27ebcd493f839f14df253541/New-Block-M-Stacked-Blue-295C_.svg","location_lng":-83.74303780000002000,"home_link":"","class_logo":"https://coursera-university-assets.s3.amazonaws.com/05/1a1f4b975bf71dba0ac79042ed651e/U-M_LogoSmHoriz_200x48.png","display":true}],"self_service_course_id":null,"short_description":"In this class, you will learn how to think with models and use them to make sense of the complex world around us.","short_name":"modelthinking","category-ids":["economics","humanities"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/modelthinking/small-icon.hover.png","instructor":"Scott E. Page","categories":[{"id":2,"name":"Economics & Finance","mailing_list_id":null,"short_name":"economics","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":6,"name":"Humanities ","mailing_list_id":11,"short_name":"humanities","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Model Thinking","language":"en","courses":[{"grading_policy_distinction":"N/A","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":null,"duration_string":"10 weeks","signature_track_last_chance_time":null,"id":30,"start_month":2,"certificate_description":"This course provided an introduction on how to think using models.  Specific topics included, among others, decision-making, tipping points, economic models, crowd dynamics, Markov processes, game theory and predictive thinking.","start_date_string":"Feb 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2013-05-13","certificates_ready":true,"signature_track_price":null,"statement_design_id":3,"signature_track_registration_open":false,"topic_id":11,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":95.9090909090909,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":1,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":95.9090909090909,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[785],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"This Statement of Accomplishment has been granted to all students who earned a grade of 70% or above.","ace_open_date":null,"home_link":"https://class.coursera.org/modelthinking/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[30],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/b5/62b8827c61b48597f9b526ce04aaee/dbLogo.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/cb/61b70b13451469aa582fc91d020f64/dbLogo.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/c9/643d403b593b865b40c64b0f6cc33c/dbLogo.jpg","video":"","university-ids":["stanford"],"id":12,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"","china_mirror":2,"favicon":"https://coursera-university-assets.s3.amazonaws.com/dc/581cda352d067023dcdcc0d9efd36e/favicon-stanford.ico","website_facebook":"","logo":"https://coursera-university-assets.s3.amazonaws.com/d8/4c69670e0826e42c6cd80b4a02b9a2/stanford.png","background_color":"","id":1,"location_city":"Palo Alto","location_country":"US","location_lat":37.44188340000000000,"location":"Palo Alto, CA, United States","primary_color":"#8c1515","abbr_name":"Stanford","website":"","description":"The Leland Stanford Junior University, commonly referred to as Stanford University or Stanford, is an American private research university located in Stanford, California on an 8,180-acre (3,310 ha) campus near Palo Alto, California, United States.","short_name":"stanford","landing_page_banner":"","mailing_list_id":null,"website_youtube":"","partner_type":1,"banner":"","location_state":"CA","name":"Stanford University","square_logo":"","square_logo_source":"","square_logo_svg":"","location_lng":-122.14301949999998000,"home_link":"http://online.stanford.edu/","class_logo":"https://coursera-university-assets.s3.amazonaws.com/21/9a0294e2bf773901afbfcb5ef47d97/Stanford_Coursera-200x48_RedText_BG.png","display":true}],"self_service_course_id":32,"short_description":"This course covers database design and the use of database management systems for applications.","short_name":"db","category-ids":["cs-systems","cs-programming"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/cb/61b70b13451469aa582fc91d020f64/dbLogo.jpg","instructor":"Jennifer Widom, Professor","categories":[{"id":11,"name":"Computer Science: Systems & Security","mailing_list_id":null,"short_name":"cs-systems","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":12,"name":"Computer Science: Software Engineering","mailing_list_id":null,"short_name":"cs-programming","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Introduction to Databases","language":"en","courses":[{"grading_policy_distinction":"","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":null,"duration_string":"","signature_track_last_chance_time":null,"id":32,"start_month":null,"certificate_description":"","start_date_string":"Self-service","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":null,"certificates_ready":false,"signature_track_price":null,"statement_design_id":null,"signature_track_registration_open":false,"topic_id":12,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":null,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":null,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":null,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":1,"start_year":null,"signature_track_certificate_combined_signature":null,"end_date":null,"notified_subscribers":true,"instructors":[1196954],"active":true,"eligible_for_certificates":false,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"2012-selfservice","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"","ace_open_date":null,"home_link":"https://class.coursera.org/db/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":null,"signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[32],"display":true},{"photo":"https://s3.amazonaws.com/coursera/topics/sna/large-icon.png","preview_link":null,"small_icon_hover":"https://s3.amazonaws.com/coursera/topics/sna/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/sna/large-icon.png","video":"AJGGiAb47S4","university-ids":["umich"],"id":38,"universities":[{"rectangular_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/75/07a2c2f614597144b29c9e63bbee75/U-M_Coursera_Logo-2.svg","wordmark":null,"website_twitter":"umich","china_mirror":3,"favicon":"https://coursera-university-assets.s3.amazonaws.com/50/52265d555e7bf0b0b86f7a1a07a183/favicon-umich.ico","website_facebook":"universityofmichigan","logo":"https://coursera-university-assets.s3.amazonaws.com/99/263203c94842c44bcf757c4a801e8f/U-M_LogoSmHoriz_160x60.png","background_color":"","id":3,"location_city":"Ann Arbor","location_country":"US","location_lat":42.28082560000000000,"location":"Ann Arbor, MI","primary_color":"#002e5f","abbr_name":"Michigan","website":"http://www.umich.edu/","description":"The University of Michigan is a public research university located in Ann Arbor, Michigan in the United States.","short_name":"umich","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/03/70c6a457583d267a869cb6555b4de5/DIL-17Apr12_AT150a_EDIT_TO_2.jpg","mailing_list_id":null,"website_youtube":"user/um","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/36/1d953d8380a15d8eb0d307c7cde0bb/banner-umich.jpg","location_state":"MI","name":"University of Michigan","square_logo":"https://coursera-university-assets.s3.amazonaws.com/70/de505d47be7d3a063b51b6f856a6e2/New-Block-M-Stacked-Blue-295C_600x600.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/26/23ecbedd1043250eab7d1b0a41696b/New-Block-M-Stacked-Blue-295C_600x600.png","square_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/6b/fce84d27ebcd493f839f14df253541/New-Block-M-Stacked-Blue-295C_.svg","location_lng":-83.74303780000002000,"home_link":"","class_logo":"https://coursera-university-assets.s3.amazonaws.com/05/1a1f4b975bf71dba0ac79042ed651e/U-M_LogoSmHoriz_200x48.png","display":true}],"self_service_course_id":null,"short_description":"This course will use social network analysis, both its theory and computational tools, to make sense of the social and information networks that have been fueled and rendered accessible by the internet.","short_name":"sna","category-ids":["infotech","cs-ai"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/sna/small-icon.hover.png","instructor":"Lada Adamic","categories":[{"id":4,"name":"Information, Tech & Design","mailing_list_id":null,"short_name":"infotech","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":17,"name":"Computer Science: Artificial Intelligence","mailing_list_id":null,"short_name":"cs-ai","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Social Network Analysis","language":"en","courses":[{"grading_policy_distinction":"The programming track is composed of two parts. The first part consists of 7 assignments (70%) and a final exam (30%) from the regular track. The second part consists of 3 programming assignments (70%) and a final project (30%). To receive a Statement of Accomplishment with Distinction, you have to obtain >= 80% of the maximum possible score on both parts individually.","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":24,"duration_string":"8 weeks","signature_track_last_chance_time":null,"id":108,"start_month":9,"certificate_description":"This interdisciplinary course uses social network analysis to understand how networks form, how they are structured, and how this structure influences processes occurring over networks.","start_date_string":"24 September 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2012-12-06","certificates_ready":true,"signature_track_price":null,"statement_design_id":3,"signature_track_registration_open":false,"topic_id":38,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":70.0,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":1,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":85.6667,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[123682],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The regular track final grade is based on 7 assignments (70% of the final grade) and a final exam (30% of the final grade). To receive a Statement of Accomplishment, you have to obtain >= 80% of the maximum possible score.","ace_open_date":null,"home_link":"https://class.coursera.org/sna-2012-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[108],"display":true},{"photo":"https://s3.amazonaws.com/coursera/topics/introfinance/large-icon.png","preview_link":null,"small_icon_hover":"https://s3.amazonaws.com/coursera/topics/introfinance/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/introfinance/large-icon.png","video":"7xy0mKUCXV4","university-ids":["umich"],"id":40,"universities":[{"rectangular_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/75/07a2c2f614597144b29c9e63bbee75/U-M_Coursera_Logo-2.svg","wordmark":null,"website_twitter":"umich","china_mirror":3,"favicon":"https://coursera-university-assets.s3.amazonaws.com/50/52265d555e7bf0b0b86f7a1a07a183/favicon-umich.ico","website_facebook":"universityofmichigan","logo":"https://coursera-university-assets.s3.amazonaws.com/99/263203c94842c44bcf757c4a801e8f/U-M_LogoSmHoriz_160x60.png","background_color":"","id":3,"location_city":"Ann Arbor","location_country":"US","location_lat":42.28082560000000000,"location":"Ann Arbor, MI","primary_color":"#002e5f","abbr_name":"Michigan","website":"http://www.umich.edu/","description":"The University of Michigan is a public research university located in Ann Arbor, Michigan in the United States.","short_name":"umich","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/03/70c6a457583d267a869cb6555b4de5/DIL-17Apr12_AT150a_EDIT_TO_2.jpg","mailing_list_id":null,"website_youtube":"user/um","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/36/1d953d8380a15d8eb0d307c7cde0bb/banner-umich.jpg","location_state":"MI","name":"University of Michigan","square_logo":"https://coursera-university-assets.s3.amazonaws.com/70/de505d47be7d3a063b51b6f856a6e2/New-Block-M-Stacked-Blue-295C_600x600.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/26/23ecbedd1043250eab7d1b0a41696b/New-Block-M-Stacked-Blue-295C_600x600.png","square_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/6b/fce84d27ebcd493f839f14df253541/New-Block-M-Stacked-Blue-295C_.svg","location_lng":-83.74303780000002000,"home_link":"","class_logo":"https://coursera-university-assets.s3.amazonaws.com/05/1a1f4b975bf71dba0ac79042ed651e/U-M_LogoSmHoriz_200x48.png","display":true}],"self_service_course_id":null,"short_description":"This course will introduce you to frameworks and tools to measure value; both for corporate and personal assets. It will also help you in decision-making, again at both the corporate and personal levels.","short_name":"introfinance","category-ids":["economics","business"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/introfinance/small-icon.hover.png","instructor":"Gautam Kaul","categories":[{"id":2,"name":"Economics & Finance","mailing_list_id":null,"short_name":"economics","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":13,"name":"Business & Management","mailing_list_id":null,"short_name":"business","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Introduction to Finance","language":"en","courses":[{"grading_policy_distinction":"   ","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":23,"duration_string":"13 weeks","signature_track_last_chance_time":null,"id":112,"start_month":7,"certificate_description":"You have successfully completed the course, Introduction to Finance, an online, non-credit course, authorized by the University of Michigan and taught by Professor Gautam Kaul of the University of Michigan.","start_date_string":"23 July 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2013-03-18","certificates_ready":true,"signature_track_price":null,"statement_design_id":3,"signature_track_registration_open":false,"topic_id":40,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":100.0,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":1,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":100.0,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[209008],"active":false,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"This student scored at least 70% on five of the nine assignments, and earned a minimum of 70% on the final exam.","ace_open_date":null,"home_link":"https://class.coursera.org/introfinance-2012-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[112],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/65/633c11334e2ae04b02afbe1edf0085/data_B-02.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/85/66e0866777387498cefc7190837922/data_B-02.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/f0/2cf694d28c1bca40bc1604b0d43c46/data_B-02.jpg","video":"-lutj1vrPwQ","university-ids":["jhu"],"id":61,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"JohnsHopkins","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/80/d81353ccd0bab30962a5770d7aec97/favicon-jhu.ico","website_facebook":"johnshopkinsuniversity","logo":"https://coursera-university-assets.s3.amazonaws.com/cf/0db65d1501244b9e6eacfa5e637fea/logo-jhu-front.png","background_color":"","id":8,"location_city":"Baltimore","location_country":"US","location_lat":39.29038480000000000,"location":"Baltimore, MD","primary_color":"#0061AA","abbr_name":"Johns Hopkins","website":"http://www.jhu.edu/","description":"The mission of The Johns Hopkins University is to educate its students and cultivate their capacity for life-long learning, to foster independent and original research, and to bring the benefits of discovery to the world.","short_name":"jhu","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/c2/544537cee93c0acd67b6573d46a32e/Coursera-banner.jpg","mailing_list_id":null,"website_youtube":"johnshopkins","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/83/94437b11dcbef874ea1f30d960a26b/banner-jhu.jpg","location_state":"MD","name":"Johns Hopkins University","square_logo":"https://coursera-university-assets.s3.amazonaws.com/4b/229e9cabab40da92cdd0fc46cd7e06/JHUNewSquare.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/1d/4b4ca39464d8fd3a75bf8a68d0a1d9/JHUNewSquare.png","square_logo_svg":"","location_lng":-76.61218930000001000,"home_link":null,"class_logo":"","display":true}],"self_service_course_id":null,"short_description":"Learn about the most effective data analysis methods to solve problems and achieve insight.","short_name":"dataanalysis","category-ids":["health","stats"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/85/66e0866777387498cefc7190837922/data_B-02.jpg","instructor":"Jeff Leek","categories":[{"id":8,"name":"Health & Society","mailing_list_id":null,"short_name":"health","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":16,"name":"Statistics and Data Analysis","mailing_list_id":null,"short_name":"stats","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Data Analysis","language":"en","courses":[{"grading_policy_distinction":"The final grade is based eight weekly quizzes worth 10 points and two peer-reviewed data analysis reports worth 40 points each. There are 160 total points for the course. To earn the statement of accomplishment with distinction for the course, a student must earn at least 144 points.","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":28,"duration_string":"8 weeks","signature_track_last_chance_time":null,"id":971332,"start_month":10,"certificate_description":"This course teaches students the most effective data analysis methods to solve problems and achieve insight.","start_date_string":null,"chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":null,"certificates_ready":false,"signature_track_price":null,"statement_design_id":6,"signature_track_registration_open":false,"topic_id":61,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":null,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":null,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":null,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":1,"start_year":2013,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[694443],"active":false,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"002","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade is based eight weekly quizzes worth 10 points and two peer-reviewed data analysis reports worth 40 points each. There are 160 total points for the course. To earn the statement of accomplishment for the course, a student must earn at least 100 points.","ace_open_date":null,"home_link":"https://class.coursera.org/dataanalysis-002/","creator_id":248174,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"https://coursera-university-assets.s3.amazonaws.com/73/45231e50250d1f9778f9c45850c362/bloomberg.small.horizontal.blue-Coursera.png","course-ids":[971332],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/9d/193d89375944329ca3c0bf32b32c56/computing_B-02.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/97/aba290f88787a8c3b13a816837ab6c/computing_B-02.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/76/469a2849f2ae5a29cbefba53c82592/computing_B-02.jpg","video":"gk6E57H6mTs","university-ids":["jhu"],"id":63,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"JohnsHopkins","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/80/d81353ccd0bab30962a5770d7aec97/favicon-jhu.ico","website_facebook":"johnshopkinsuniversity","logo":"https://coursera-university-assets.s3.amazonaws.com/cf/0db65d1501244b9e6eacfa5e637fea/logo-jhu-front.png","background_color":"","id":8,"location_city":"Baltimore","location_country":"US","location_lat":39.29038480000000000,"location":"Baltimore, MD","primary_color":"#0061AA","abbr_name":"Johns Hopkins","website":"http://www.jhu.edu/","description":"The mission of The Johns Hopkins University is to educate its students and cultivate their capacity for life-long learning, to foster independent and original research, and to bring the benefits of discovery to the world.","short_name":"jhu","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/c2/544537cee93c0acd67b6573d46a32e/Coursera-banner.jpg","mailing_list_id":null,"website_youtube":"johnshopkins","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/83/94437b11dcbef874ea1f30d960a26b/banner-jhu.jpg","location_state":"MD","name":"Johns Hopkins University","square_logo":"https://coursera-university-assets.s3.amazonaws.com/4b/229e9cabab40da92cdd0fc46cd7e06/JHUNewSquare.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/1d/4b4ca39464d8fd3a75bf8a68d0a1d9/JHUNewSquare.png","square_logo_svg":"","location_lng":-76.61218930000001000,"home_link":null,"class_logo":"","display":true}],"self_service_course_id":null,"short_description":"This course is about learning the fundamental computing skills necessary for effective data analysis. You will learn to program in R and to use R for reading data, writing functions, making informative graphs, and applying modern statistical methods.","short_name":"compdata","category-ids":["health","stats"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/97/aba290f88787a8c3b13a816837ab6c/computing_B-02.jpg","instructor":"Roger Peng","categories":[{"id":8,"name":"Health & Society","mailing_list_id":null,"short_name":"health","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":16,"name":"Statistics and Data Analysis","mailing_list_id":null,"short_name":"stats","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Computing for Data Analysis","language":"en","courses":[{"grading_policy_distinction":"The final grade was based on four quizzes and two programming assignments. To receive a Statement of Accomplishments, students must obtain at least 90% of the maximum possible score.","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":24,"duration_string":"4 weeks","signature_track_last_chance_time":null,"id":300,"start_month":9,"certificate_description":"In this course students learn programming in R, reading data into R, creating data graphics, accessing and installing R packages, writing R functions, debugging, and organizing and commenting R code. ","start_date_string":"24 September 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2012-12-06","certificates_ready":true,"signature_track_price":null,"statement_design_id":6,"signature_track_registration_open":false,"topic_id":63,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":100.0,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":2,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":100.0,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":null,"end_date":null,"notified_subscribers":true,"instructors":[685384],"active":false,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on four quizzes and two programming assignments. To receive a Statement of Accomplishments, students must obtain at least 70% of the maximum possible score.","ace_open_date":null,"home_link":"https://class.coursera.org/compdata-2012-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"https://coursera-university-assets.s3.amazonaws.com/46/2952c73da820e080b97b63f9ac0fba/bloomberg.small.horizontal.blue-Coursera.png","course-ids":[300],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/eb/cf8c0e7e0bb418255e48fe210446c1/bootcamp_B-02.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/cd/3cc4db194a8f70139ec63afd200a0f/bootcamp_B-02.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/99/b022103b7d744911d090acafe2cb98/bootcamp_B-02.jpg","video":"ekdpaf_WT_8","university-ids":["jhu"],"id":68,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"JohnsHopkins","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/80/d81353ccd0bab30962a5770d7aec97/favicon-jhu.ico","website_facebook":"johnshopkinsuniversity","logo":"https://coursera-university-assets.s3.amazonaws.com/cf/0db65d1501244b9e6eacfa5e637fea/logo-jhu-front.png","background_color":"","id":8,"location_city":"Baltimore","location_country":"US","location_lat":39.29038480000000000,"location":"Baltimore, MD","primary_color":"#0061AA","abbr_name":"Johns Hopkins","website":"http://www.jhu.edu/","description":"The mission of The Johns Hopkins University is to educate its students and cultivate their capacity for life-long learning, to foster independent and original research, and to bring the benefits of discovery to the world.","short_name":"jhu","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/c2/544537cee93c0acd67b6573d46a32e/Coursera-banner.jpg","mailing_list_id":null,"website_youtube":"johnshopkins","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/83/94437b11dcbef874ea1f30d960a26b/banner-jhu.jpg","location_state":"MD","name":"Johns Hopkins University","square_logo":"https://coursera-university-assets.s3.amazonaws.com/4b/229e9cabab40da92cdd0fc46cd7e06/JHUNewSquare.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/1d/4b4ca39464d8fd3a75bf8a68d0a1d9/JHUNewSquare.png","square_logo_svg":"","location_lng":-76.61218930000001000,"home_link":null,"class_logo":"","display":true}],"self_service_course_id":null,"short_description":"This class presents the fundamental probability and statistical concepts used in elementary data analysis. It will be taught at an introductory level for students with junior or senior college-level mathematical training including a working knowledge of calculus. A small amount of linear algebra and programming are useful for the class, but not required. ","short_name":"biostats","category-ids":["math","health","biology","stats"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/cd/3cc4db194a8f70139ec63afd200a0f/bootcamp_B-02.jpg","instructor":"Brian Caffo","categories":[{"id":5,"name":"Mathematics","mailing_list_id":null,"short_name":"math","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":8,"name":"Health & Society","mailing_list_id":null,"short_name":"health","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":10,"name":"Biology & Life Sciences","mailing_list_id":null,"short_name":"biology","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":16,"name":"Statistics and Data Analysis","mailing_list_id":null,"short_name":"stats","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Mathematical Biostatistics Boot Camp 1","language":"en","courses":[{"grading_policy_distinction":"The final grade was based on a series of 7 weekly assessments. To receive a Statement of Accomplishment, students must obtain at least 90% of the maximum possible score.","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":24,"duration_string":"7 weeks","signature_track_last_chance_time":null,"id":302,"start_month":9,"certificate_description":"This course puts forward key mathematical and statistical topics to help students understand biostatistics at a deeper level. Successful students have a basic understanding of the goals, assumptions, benefits and negatives of probability modeling in the medical sciences.","start_date_string":"24 September 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2012-12-11","certificates_ready":true,"signature_track_price":null,"statement_design_id":6,"signature_track_registration_open":false,"topic_id":68,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":94.7619047619047,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":2,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":94.7619047619047,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":null,"end_date":null,"notified_subscribers":true,"instructors":[688901],"active":false,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on a series of 7 weekly assessments. To receive a Statement of Accomplishment, students must obtain at least 70% of the maximum possible score.","ace_open_date":null,"home_link":"https://class.coursera.org/biostats-2012-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"https://coursera-university-assets.s3.amazonaws.com/41/41ed34d097fda979808abc09e4030a/bloomberg.small.horizontal.blue-Coursera.png","course-ids":[302],"display":true},{"photo":"https://s3.amazonaws.com/coursera/topics/neuralnets/large-icon.png","preview_link":null,"small_icon_hover":"https://s3.amazonaws.com/coursera/topics/neuralnets/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/neuralnets/large-icon.png","video":"KuPai0ogiHk","university-ids":["utoronto"],"id":77,"universities":[{"rectangular_logo_svg":null,"wordmark":null,"website_twitter":"uoftnews","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/8e/773801b870a0cb946698134a77417d/favicon-utoronto.ico","website_facebook":"universitytoronto","logo":"https://coursera-university-assets.s3.amazonaws.com/78/334b4573e06c876cff8e2484f082d7/logo-utoronto-front.png","background_color":"","id":11,"location_city":"Toronto","location_country":"CA","location_lat":43.65322600000000000,"location":"Toronto, ON, Canada","primary_color":"#1c3361","abbr_name":"U of T","website":"http://www.utoronto.ca/","description":"Established in 1827, the University of Toronto has one of the strongest research and teaching faculties in North America, presenting top students at all levels with an intellectual environment unmatched in depth and breadth on any other Canadian campus.","short_name":"utoronto","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/84/774219ef720c726661c682cda7838a/UofT-Banner.jpg","mailing_list_id":null,"website_youtube":"universitytoronto","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/6f/9a578312b814f8aeaf942f4f248447/banner-utoronto.jpg","location_state":"ON","name":"University of Toronto","square_logo":"https://coursera-university-assets.s3.amazonaws.com/04/1df8943d27a485a986a3ebf10c83d9/UofT-Crest-Square.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/dd/cf4561b10afb34d449bb9574ecbca3/UofT-Crest-Square.png","square_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/06/0e2fff91e34df097bbb12ecd57c0af/UofT-Crest-Square.svg","location_lng":-79.38318429999998000,"home_link":null,"class_logo":"https://coursera-university-assets.s3.amazonaws.com/e0/c3219157fdcd439fb3b765c4b723e5/UofT-Crest-Wide.png","display":true}],"self_service_course_id":null,"short_description":"Learn about artificial neural networks and how they're being used for machine learning, as applied to speech and object recognition, image segmentation, modeling language and human motion, etc. We'll emphasize both the basic algorithms and the practical tricks needed to get them to work well.","short_name":"neuralnets","category-ids":["stats","cs-ai"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/neuralnets/small-icon.hover.png","instructor":"Geoffrey Hinton","categories":[{"id":16,"name":"Statistics and Data Analysis","mailing_list_id":null,"short_name":"stats","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":17,"name":"Computer Science: Artificial Intelligence","mailing_list_id":null,"short_name":"cs-ai","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Neural Networks for Machine Learning","language":"en","courses":[{"grading_policy_distinction":"Students who achieved at least 90% overall receive a Statement of Accomplishment with Distinction.\n","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":1,"duration_string":"8 weeks","signature_track_last_chance_time":null,"id":256,"start_month":10,"certificate_description":"The course covered learning techniques for many different types of neural network including deep feed-forward networks, recurrent networks and Boltzmann Machines. It covered recent applications to speech, vision, and language, and used hands-on programming assignments.","start_date_string":"24 September 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2012-12-19","certificates_ready":true,"signature_track_price":null,"statement_design_id":14,"signature_track_registration_open":false,"topic_id":77,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":70.2813657407407,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":1,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":70.2813657407407,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":1,"start_year":2012,"signature_track_certificate_combined_signature":null,"end_date":null,"notified_subscribers":true,"instructors":[831097],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on the four programming assignments (5% for PA1, 10% for each of the other three), the weekly quizzes (40%, with each quiz being weighted as one fifteenth of that), and the final exam (25%). Students who achieved at least 70% overall receive a Statement of Accomplishment.\n","ace_open_date":null,"home_link":"https://class.coursera.org/neuralnets-2012-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[256],"display":true},{"photo":"https://s3.amazonaws.com/coursera/topics/datasci/large-icon.png","preview_link":"https://class.coursera.org/datasci-001/lecture/preview","small_icon_hover":"https://s3.amazonaws.com/coursera/topics/datasci/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/datasci/large-icon.png","video":"","university-ids":["uw"],"id":106,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"UW","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/98/7480c1f9271144aa75fe024a1ad5a0/favicon-uw.ico","website_facebook":"UofWA","logo":"https://coursera-university-assets.s3.amazonaws.com/53/2f0ff155b3390e3d7190c363a4a045/uw-logo-coursera-transparent.png","background_color":"","id":15,"location_city":"Seattle","location_country":"US","location_lat":47.60620950000000000,"location":"Seattle, WA","primary_color":"#38255a","abbr_name":"Washington","website":"https://www.washington.edu/","description":"Founded in 1861, the University of Washington is one of the oldest state-supported institutions of higher education on the West Coast and is one of the preeminent research universities in the world.","short_name":"uw","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/d0/f1be9c6b196a5a0446da96548a85b6/banner-uw1.jpg","mailing_list_id":null,"website_youtube":"user/uwhuskies","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/5d/7f56a74b7b3009b4a888e372a41f67/banner-uw1.jpg","location_state":"WA","name":"University of Washington","square_logo":"https://coursera-university-assets.s3.amazonaws.com/8a/903cf68039c31a207eb7eafff458f7/w-patch-purple.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/76/45ce3d3eba0b28c763eacbb2387322/w-patch-purple.png","square_logo_svg":"","location_lng":-122.33207080000000000,"home_link":null,"class_logo":"","display":true}],"self_service_course_id":null,"short_description":"Join the data revolution. Companies are searching for data scientists. This specialized field demands multiple skills not easy to obtain through conventional curricula. Introduce yourself to the basics of data science and leave armed with practical experience extracting value from big data.","short_name":"datasci","category-ids":["infotech","cs-systems","cs-programming","stats"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/datasci/small-icon.hover.png","instructor":"Bill Howe","categories":[{"id":4,"name":"Information, Tech & Design","mailing_list_id":null,"short_name":"infotech","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":11,"name":"Computer Science: Systems & Security","mailing_list_id":null,"short_name":"cs-systems","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":12,"name":"Computer Science: Software Engineering","mailing_list_id":null,"short_name":"cs-programming","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":16,"name":"Statistics and Data Analysis","mailing_list_id":null,"short_name":"stats","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Introduction to Data Science","language":"en","courses":[{"grading_policy_distinction":"The final grade was based on satisfactory completion of three required programming assignments, a peer-assessed competition in predictive analytics, a peer-assessed visualization assessment, and participation in the forums for peer assistance and optional projects.   To receive a Statement of Accomplishment, a student was required to obtain >= 75% of the maximum possible score. ","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":1,"duration_string":"8 weeks","signature_track_last_chance_time":null,"id":346,"start_month":5,"certificate_description":"This course covered a broad set of topics critical to practical data science: relational databases, MapReduce, NoSQL, statistical modeling, basic machine learning, and visualization, and a variety of algorithmic topics.","start_date_string":"April 2013","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2013-06-29","certificates_ready":true,"signature_track_price":null,"statement_design_id":43,"signature_track_registration_open":false,"topic_id":106,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":58.1818181818182,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":0,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":58.1818181818182,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2013,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[999946],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on satisfactory completion of three required programming assignments, a peer-assessed competition in predictive analytics, a peer-assessed visualization assessment, and participation in the forums for peer assistance and optional projects.   To receive a Statement of Accomplishment, a student was required to obtain >= 60% of the maximum possible score. ","ace_open_date":null,"home_link":"https://class.coursera.org/datasci-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":null,"course-ids":[346],"display":true},{"photo":"https://s3.amazonaws.com/coursera/topics/progfun/large-icon.png","preview_link":null,"small_icon_hover":"https://s3.amazonaws.com/coursera/topics/progfun/small-icon.hover.png","large_icon":"https://s3.amazonaws.com/coursera/topics/progfun/large-icon.png","video":"_NVySmdAH4c","university-ids":["epfl"],"id":116,"universities":[{"rectangular_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/89/7f14b8c6f54b56a2446c99b2b8df86/EPFL_LOG_QUADRI_Red_resized.svg","wordmark":null,"website_twitter":"EPFL","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/ba/142188830b51c8c51b95cc25e5dbf3/epflfav.png","website_facebook":"EPFL.ch","logo":"https://coursera-university-assets.s3.amazonaws.com/1d/0a73c737b332745aac47797f57cda4/logo-epfl-front.png","background_color":"","id":16,"location_city":"Lausanne","location_country":"CH","location_lat":46.51996170000000000,"location":"Lausanne, Switzerland","primary_color":"#7E000C","abbr_name":"EPFL","website":"http://www.epfl.ch/","description":"","short_name":"epfl","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/22/b50d471bc34471c5a7c696b1d714cc/rolex_learning_center.jpg","mailing_list_id":null,"website_youtube":"user/epflnews","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/ab/bb616e071f67f5bcaead9b67dbaa36/banner-epfl.jpg","location_state":"VD","name":"\u00c9cole Polytechnique F\u00e9d\u00e9rale de Lausanne","square_logo":"https://coursera-university-assets.s3.amazonaws.com/01/a5c9c211331132096e7ba9942dbd51/square_epfl_400.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/5f/f441e9c1630625018763c3b86cf582/square_epfl_400.png","square_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/a7/fca9be507583b1f25e0fcd9fca0d59/square_epfl_400.svg","location_lng":6.63359709999997450,"home_link":null,"class_logo":"","display":true}],"self_service_course_id":null,"short_description":"Learn about functional programming, and how it can be effectively combined with object-oriented programming. Gain practice in writing clean functional code, using the Scala programming language.","short_name":"progfun","category-ids":["cs-programming"],"visibility":0,"small_icon":"https://s3.amazonaws.com/coursera/topics/progfun/small-icon.hover.png","instructor":"Martin Odersky","categories":[{"id":12,"name":"Computer Science: Software Engineering","mailing_list_id":null,"short_name":"cs-programming","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Functional Programming Principles in Scala","language":"en","courses":[{"grading_policy_distinction":"The final grade was based on 6 programming assignments. To receive a Statement of Accomplishment with Distinction, one must obtain >= 80% of the maximum possible score.","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":18,"duration_string":"7 weeks","signature_track_last_chance_time":null,"id":308,"start_month":9,"certificate_description":"This advanced undergraduate programming course covers the principles of functional programming using Scala, including the use of functions as values, recursion, immutability, pattern matching, higher-order functions and collections, and lazy evaluation.","start_date_string":"18 September 2012","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2012-12-05","certificates_ready":true,"signature_track_price":null,"statement_design_id":13,"signature_track_registration_open":false,"topic_id":116,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":98.0,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":2,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":98.0,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2012,"signature_track_certificate_combined_signature":null,"end_date":null,"notified_subscribers":true,"instructors":[672627],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":null,"grading_policy_normal":"The final grade was based on 6 programming assignments. To receive a Statement of Accomplishment, one must obtain >= 60% of the maximum possible score.","ace_open_date":null,"home_link":"https://class.coursera.org/progfun-2012-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[308],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/02/c44a1233fcf277511c7720292324e3/course_logo.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/e1/644d37da2af639d0c9d1f4fca323c2/course_logo.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/f2/e2d37549671b4a0c04b4f40669e1c9/course_logo.jpg","video":"","university-ids":["uw"],"id":117,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"UW","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/98/7480c1f9271144aa75fe024a1ad5a0/favicon-uw.ico","website_facebook":"UofWA","logo":"https://coursera-university-assets.s3.amazonaws.com/53/2f0ff155b3390e3d7190c363a4a045/uw-logo-coursera-transparent.png","background_color":"","id":15,"location_city":"Seattle","location_country":"US","location_lat":47.60620950000000000,"location":"Seattle, WA","primary_color":"#38255a","abbr_name":"Washington","website":"https://www.washington.edu/","description":"Founded in 1861, the University of Washington is one of the oldest state-supported institutions of higher education on the West Coast and is one of the preeminent research universities in the world.","short_name":"uw","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/d0/f1be9c6b196a5a0446da96548a85b6/banner-uw1.jpg","mailing_list_id":null,"website_youtube":"user/uwhuskies","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/5d/7f56a74b7b3009b4a888e372a41f67/banner-uw1.jpg","location_state":"WA","name":"University of Washington","square_logo":"https://coursera-university-assets.s3.amazonaws.com/8a/903cf68039c31a207eb7eafff458f7/w-patch-purple.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/76/45ce3d3eba0b28c763eacbb2387322/w-patch-purple.png","square_logo_svg":"","location_lng":-122.33207080000000000,"home_link":null,"class_logo":"","display":true}],"self_service_course_id":null,"short_description":"Investigate the basic concepts behind programming languages, with a strong emphasis on the techniques and benefits of functional programming. Use the programming languages ML, Racket, and Ruby in ways that will teach you how the pieces of a language fit together to create more than the sum of the parts. Gain new software skills and the concepts needed to learn new languages on your own.","short_name":"proglang","category-ids":["cs-programming"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/e1/644d37da2af639d0c9d1f4fca323c2/course_logo.jpg","instructor":"Dan  Grossman","categories":[{"id":12,"name":"Computer Science: Software Engineering","mailing_list_id":null,"short_name":"cs-programming","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Programming Languages","language":"en","courses":[{"grading_policy_distinction":"","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":14,"duration_string":"10 weeks","signature_track_last_chance_time":null,"id":352,"start_month":1,"certificate_description":"This course investigates the basic concepts behind programming languages, with a strong emphasis on the techniques and benefits of functional programming along with many other topics.","start_date_string":"January 2013","chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2013-04-02","certificates_ready":true,"signature_track_price":null,"statement_design_id":73,"signature_track_registration_open":false,"topic_id":117,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":100.0,"share_for_work":false,"is_enrolled_for_proctored_exam":false,"achievement_level":1,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":83.2487076923077,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2013,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[873260],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"12-001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on 7 assignments each worth 10% of the final grade, with 90% of the 10% produced from auto-grading and the remaining 10% coming from peer assessment.  The course had two exams each worth 15% of the course grade.","ace_open_date":null,"home_link":"https://class.coursera.org/proglang-2012-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":"","signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[352],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/ec/6faacf42b19cf2772061ae095d98ee/460_259_logo.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/c9/8b3a8357fe29791971ece8cc8fb35b/460_259_logo.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/6c/ffdfc097878bc0f8115148d7ee6826/460_259_logo.jpg","video":"IWugXcWpfoM","university-ids":["brown"],"id":198,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"BrownUniversity","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/54/51fc6594a22ce0fcd31168f109900f/5_brown_icon_coursera.jpg","website_facebook":"BrownUniversity","logo":"https://coursera-university-assets.s3.amazonaws.com/1e/b5f0ff349ddbf16b116d9a181c9552/Coursera_CourseTopLeftLogo_Brown.png","background_color":"","id":22,"location_city":"","location_country":"","location_lat":null,"location":"Providence, RI","primary_color":"#c00404","abbr_name":"Brown","website":"http://www.brown.edu/","description":"Founded in 1764, Brown University is an independent, coeducational Ivy League institution. It is recognized for the quality of its teaching, research, and unique curriculum. The seventh-oldest college in the United States, Brown University is located in historic Providence, Rhode Island.","short_name":"brown","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/c0/b026ace077f637396a1dacdf1eb779/3_banner6_coursera.jpg","mailing_list_id":null,"website_youtube":"brownuniversity","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/7f/dcd11ef97345d4931ed1d5b187f21e/2_brown_banner_coursera.jpg","location_state":"","name":"Brown University","square_logo":"https://coursera-university-assets.s3.amazonaws.com/c6/d5fc95810a4783b7f71c066c620a54/1_brown_logo_coursera.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/b9/5c77b26d99a8c59314414d6d43e3e2/1_brown_logo_coursera.png","square_logo_svg":"","location_lng":null,"home_link":null,"class_logo":"https://coursera-university-assets.s3.amazonaws.com/1a/474b90e9993ebe86bea13056066d66/8_brown_logo_class_page_coursera.png","display":true}],"self_service_course_id":null,"short_description":"Learn the concepts and methods of linear algebra, and how to use them to think about computational problems arising in computer science.  Coursework includes building on the concepts to write small programs and run them on real data. ","short_name":"matrix","category-ids":["cs-theory","math","stats","cs-ai"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/c9/8b3a8357fe29791971ece8cc8fb35b/460_259_logo.jpg","instructor":"Phil Klein","categories":[{"id":1,"name":"Computer Science: Theory","mailing_list_id":null,"short_name":"cs-theory","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":5,"name":"Mathematics","mailing_list_id":null,"short_name":"math","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":16,"name":"Statistics and Data Analysis","mailing_list_id":null,"short_name":"stats","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":17,"name":"Computer Science: Artificial Intelligence","mailing_list_id":null,"short_name":"cs-ai","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Coding the Matrix: Linear Algebra through Computer Science Applications","language":"en","courses":[{"grading_policy_distinction":"","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":1,"duration_string":"8 weeks","signature_track_last_chance_time":null,"id":970260,"start_month":7,"certificate_description":"","start_date_string":null,"chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":null,"certificates_ready":false,"signature_track_price":null,"statement_design_id":null,"signature_track_registration_open":false,"topic_id":198,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":null,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":null,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":null,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":0,"start_year":2013,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[1234717],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"","ace_open_date":null,"home_link":"https://class.coursera.org/matrix-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":null,"signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[970260],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/26/761dfb7b58773c88fe4cb2aed26a3c/iStock_000020359734_Small.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/c5/42ab154bbf6f76f51b74b8347deab5/iStock_000020359734_Small.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/7e/0150790f6c92c48d03bf6178ea9193/iStock_000020359734_Small.jpg","video":"","university-ids":["columbia"],"id":225,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"columbia","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/72/c9b56e3ff66fa6e1ca52bdea9440ae/ColumbialittleLOGO4.png","website_facebook":"Columbia","logo":"https://coursera-university-assets.s3.amazonaws.com/9b/42f2eca2f10769400d1f8519892413/ColumbiaLOGO.png","background_color":"","id":40,"location_city":"New York","location_country":"US","location_lat":40.71435280000000000,"location":"New York City, NY, USA","primary_color":"#1f2f60","abbr_name":"Columbia","website":"http://www.columbia.edu/","description":"For more than 250 years, Columbia has been a leader in higher education in the nation and around the world. At the core of our wide range of academic inquiry is the commitment to attract and engage the best minds in pursuit of greater human understanding, pioneering new discoveries and service to society.","short_name":"columbia","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/3d/731d687dff430714ea6413c8b9d932/banner_coursera_columbia.jpg","mailing_list_id":null,"website_youtube":"columbia","partner_type":1,"banner":"https://coursera-university-assets.s3.amazonaws.com/0d/6b60ecae321cd9d6e02b5a1376e7cb/columbia-banner.jpg","location_state":"NY","name":"Columbia University","square_logo":"https://coursera-university-assets.s3.amazonaws.com/d2/74c67a99b3e2516e7efbb4d9892721/cu_collegiate_blue.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/bb/d42540e622a1bb89a1b5f2258f3602/cu_collegiate_blue.png","square_logo_svg":"","location_lng":-74.00597310000000000,"home_link":null,"class_logo":"https://coursera-university-assets.s3.amazonaws.com/8e/592ba6a9789657a5a0912204491332/logo_cu_background_trans.png","display":true}],"self_service_course_id":null,"short_description":"Have you ever wondered how to build a system that automatically translates between languages? Or a system that can understand natural language instructions from a human?  This class will cover the fundamentals of mathematical and computational models of language, and the application of these models to key problems in natural language processing.","short_name":"nlangp","category-ids":["cs-ai"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/c5/42ab154bbf6f76f51b74b8347deab5/iStock_000020359734_Small.jpg","instructor":"Michael Collins","categories":[{"id":17,"name":"Computer Science: Artificial Intelligence","mailing_list_id":null,"short_name":"cs-ai","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Natural Language Processing","language":"en","courses":[{"grading_policy_distinction":"The final grade was based on 4 quizzes (50% of the final grade) and 3 programming\nassignments (50% of the final grade). To receive a distinction in the class, you have to obtain >= 75% of the maximum possible score.","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":24,"duration_string":"10 weeks","signature_track_last_chance_time":null,"id":970276,"start_month":2,"certificate_description":"This introductory graduate/advanced undergraduate course introduces mathematical and computational models of language, and the application of these models to key problems in natural language processing.","start_date_string":null,"chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":"2013-05-15","certificates_ready":true,"signature_track_price":null,"statement_design_id":59,"signature_track_registration_open":false,"topic_id":225,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":8.92857142857143,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":0,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":8.92857142857143,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":1,"start_year":2013,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[1325520],"active":true,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on 4 quizzes (50% of the final grade) and 3 programming\nassignments (50% of the final grade). To receive a Statement of Accomplishment,\nyou have to obtain >= 50% of the maximum possible score.","ace_open_date":null,"home_link":"https://class.coursera.org/nlangp-001/","creator_id":null,"proctored_exam_completion_date":null,"university_logo":null,"signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[970276],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/b1/0dd0ff0e44a763cfba188a761ae2cb/movielens.jpg","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/42/291d55c2b40660fdbf65e590831949/movielens.jpg","large_icon":"https://coursera-course-photos.s3.amazonaws.com/9b/6095ca516e757248df2abf357fde72/movielens.jpg","video":"eMlcMAa0IOg","university-ids":["minnesota"],"id":746,"universities":[{"rectangular_logo_svg":"https://coursera-university-assets.s3.amazonaws.com/32/051960873fd1cc2b6a315b80853ff2/minnesota.svg","wordmark":null,"website_twitter":"UMNews","china_mirror":1,"favicon":"https://coursera-university-assets.s3.amazonaws.com/f2/c0c3766390def44d6704f82d4d7955/favicon.ico","website_facebook":"UofMN","logo":"","background_color":"","id":50,"location_city":"Minneapolis","location_country":"US","location_lat":44.98333400000000000,"location":"Minneapolis, MN, USA","primary_color":"#7A0019","abbr_name":"Minnesota","website":"http://www.umn.edu/","description":"The University of Minnesota is among the largest public research universities in the country, offering undergraduate, graduate, and professional students a multitude of opportunities for study and research. Located at the heart of one of the nation\u2019s most vibrant, diverse metropolitan communities, students on the campuses in Minneapolis and St. Paul benefit from extensive partnerships with world-renowned health centers, international corporations, government agencies, and arts, nonprofit, and public service organizations.","short_name":"minnesota","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/c0/5512775b06b7e0c1b2e30d8b7585df/5-1280x320.jpg","mailing_list_id":null,"website_youtube":"UniversityofMinn","partner_type":1,"banner":"","location_state":"MN","name":"University of Minnesota","square_logo":"https://coursera-university-assets.s3.amazonaws.com/75/a144950f75d01464eef8e64003285c/FB-GoldM-maroon.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/4e/bcdf60eb515f6611860f25ca6dc893/FB-GoldM-maroon.png","square_logo_svg":"","location_lng":-93.26666999999998000,"home_link":"","class_logo":"https://coursera-university-assets.s3.amazonaws.com/2e/0cd5e2383ba389999647887f51de4c/UofMD2D200x48transp.png","display":true}],"self_service_course_id":null,"short_description":"This course introduces the concepts, applications, algorithms, programming, and design of recommender systems--software systems that recommend products or information, often based on extensive personalization.  Learn how web merchants such as Amazon.com personalize product suggestions and how to apply the same techniques in your own systems!","short_name":"recsys","category-ids":["infotech","business","cs-ai"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/42/291d55c2b40660fdbf65e590831949/movielens.jpg","instructor":null,"categories":[{"id":4,"name":"Information, Tech & Design","mailing_list_id":null,"short_name":"infotech","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":13,"name":"Business & Management","mailing_list_id":null,"short_name":"business","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"},{"id":17,"name":"Computer Science: Artificial Intelligence","mailing_list_id":null,"short_name":"cs-ai","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Introduction to Recommender Systems","language":"en","courses":[{"grading_policy_distinction":"","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":3,"duration_string":"14 weeks","signature_track_last_chance_time":null,"id":971201,"start_month":9,"certificate_description":"","start_date_string":null,"chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":null,"certificates_ready":false,"signature_track_price":null,"statement_design_id":56,"signature_track_registration_open":false,"topic_id":746,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":null,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":null,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":null,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":1,"start_year":2013,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[3108924,3691077],"active":false,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"","ace_open_date":null,"home_link":"https://class.coursera.org/recsys-001/","creator_id":952027,"proctored_exam_completion_date":null,"university_logo":null,"signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"","course-ids":[971201],"display":true},{"photo":"https://coursera-course-photos.s3.amazonaws.com/f4/fc7a006d2bafe837b6de5016d8a423/Course-Logo.png","preview_link":null,"small_icon_hover":"https://coursera-course-photos.s3.amazonaws.com/4e/1d7362a932b2aa5baaa895d4f8425b/Course-Logo.png","large_icon":"https://coursera-course-photos.s3.amazonaws.com/f0/377e1431e27761059cf1d408e67642/Course-Logo.png","video":"Bc1-PZ5NfRs","university-ids":["cusystem"],"id":781,"universities":[{"rectangular_logo_svg":"","wordmark":null,"website_twitter":"","china_mirror":1,"favicon":"","website_facebook":"","logo":"","background_color":"","id":126,"location_city":"","location_country":"","location_lat":null,"location":"","primary_color":"","abbr_name":"CU System","website":"","description":"The University of Colorado is a recognized leader in higher education on the national and global stage.  We collaborate to meet the diverse needs of our students and communities.  We promote innovation, encourage discovery and support the extension of knowledge in ways unique to the state of Colorado and beyond.","short_name":"cusystem","landing_page_banner":"https://coursera-university-assets.s3.amazonaws.com/40/7f8dc97c973fcd38a4d202c143f2a9/cu-bkg-coursera.jpg","mailing_list_id":null,"website_youtube":"","partner_type":2,"banner":"","location_state":"","name":"University of Colorado System","square_logo":"https://coursera-university-assets.s3.amazonaws.com/1b/fb8b2887a17b007626846d08988b39/colorado.png","square_logo_source":"https://coursera-university-assets.s3.amazonaws.com/0c/5859039090ad9fd55834fde43636a2/colorado.jpg","square_logo_svg":"","location_lng":null,"home_link":"","class_logo":"","display":true}],"self_service_course_id":null,"short_description":"Start learning how to program video games using the C# programming language. Plenty of practice opportunities are included!","short_name":"gameprogramming","category-ids":["cs-programming"],"visibility":0,"small_icon":"https://coursera-course-photos.s3.amazonaws.com/4e/1d7362a932b2aa5baaa895d4f8425b/Course-Logo.png","instructor":null,"categories":[{"id":12,"name":"Computer Science: Software Engineering","mailing_list_id":null,"short_name":"cs-programming","description":"Our wide range of courses allows students to explore topics from many different fields of study. Sign up for a class today and join our global community of students and scholars!"}],"name":"Beginning Game Programming with C#","language":"en","courses":[{"grading_policy_distinction":"","ace_track_price_display":null,"signature_track_certificate_design_id":null,"ace_semester_hours":null,"start_day":16,"duration_string":"8 weeks","signature_track_last_chance_time":null,"id":971200,"start_month":9,"certificate_description":"This introductory undergraduate course teaches beginning programming concepts in a game development context.","start_date_string":null,"chegg_session_id":"","signature_track_regular_price":null,"grades_release_date":null,"certificates_ready":false,"signature_track_price":null,"statement_design_id":null,"signature_track_registration_open":false,"topic_id":781,"eligible_for_signature_track":false,"start_date":null,"record":{"grade_distinction":null,"share_for_work":null,"is_enrolled_for_proctored_exam":false,"achievement_level":null,"signature_track":false,"passed_ace":false,"ace_grade":0,"grade_normal":null,"verify_cert_id":"","authenticated_overall":false,"with_grade_cert_id":""},"status":1,"start_year":2013,"signature_track_certificate_combined_signature":"","end_date":null,"notified_subscribers":true,"instructors":[3134388],"active":false,"eligible_for_certificates":true,"signature_track_certificate_signature_blurb":"","deployed":true,"ace_close_date":null,"name":"001","textbooks":[],"signature_track_open_time":null,"eligible_for_ACE":false,"grading_policy_normal":"The final grade was based on 6 Programming Assignments (48% of the final grade), a game development project (30% of the final grade), and a final exam (22% of the final grade). To receive a Statement of Accomplishment, you have to obtain >= 70% of the maximum possible score. ","ace_open_date":null,"home_link":"https://class.coursera.org/gameprogramming-001/","creator_id":3134388,"proctored_exam_completion_date":null,"university_logo":null,"signature_track_close_time":null,"auth_review_completion_date":null}],"university_logo":"https://coursera-university-assets.s3.amazonaws.com/13/0e2b677ebb94e8d3fdcd7cfdae92fd/cu-coursera.png","course-ids":[971200],"display":true}]"""

    let p_root          = Details.p_root
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
//    runTestCases ()
//    runPerformanceTestCases ()
    runPerformanceRun ()
    0
