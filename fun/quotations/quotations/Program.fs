
open System.Diagnostics

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.QuotationEvaluation

module QParser =
    open OptimizedClosures

    type CharStream<'UserState>(input : string, userState : 'UserState) =

        let mutable position            = 0
        let mutable noErrorMessages     = true

        let skipTemplate (charTest : Expr<char->int->bool>)= 
            <@
                let i           = input
                let length      = i.Length

                let mutable pos  = position
                let mutable iter = 0

                while pos < length && (%charTest) i.[pos] iter do
                    pos     <- pos + 1
                    iter    <- iter + 1

                position <- pos
                
            @>

        member x.StateTag               = position
        member x.Input                  = input
        member x.Position               = position
        member x.UserState              = userState
        member x.IsEndOfStream          = position >= input.Length || position < 0
        member x.NoErrorMessages        = noErrorMessages
        member x.Peek ()                = if x.IsEndOfStream then '\uFFFF' else input.[position]

        member x.SetPosition pos        = position <- pos
        member x.SetNoErrorMessages flag= noErrorMessages <- flag


        member x.MakeSkip (charTest : Expr<char->int->bool>) =
            (skipTemplate charTest).Compile ()

module UParser =
    open OptimizedClosures

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

        member x.StateTag               = position
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

        member x.Skip (test : CharTest) : unit =

            let i           = input
            let length      = i.Length

            let mutable pos  = position
            let mutable iter = 0

            while pos < length && test.Invoke(i.[pos],iter) do
                pos     <- pos + 1
                iter    <- iter + 1

            position <- pos

        member x.Skip2 (test : char->int->bool) : unit =

            let i           = input
            let length      = i.Length

            let mutable pos  = position
            let mutable iter = 0

            while pos < length && test i.[pos] iter do
                pos     <- pos + 1
                iter    <- iter + 1

            position <- pos

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
    [<Struct>]
    type Reply<'Result>(isOk : bool, result : 'Result, errorMessages : ErrorMessage list) =

        member x.Ok                 = isOk
        member x.Error              = not isOk
        member x.Result             = result
        member x.ErrorMessages      = errorMessages

    type Parser<'Result, 'UserState> = CharStream<'UserState> -> Reply<'Result>

    let inline success (v : 'T) ems    = Reply<'T> (true, v, ems)
    let inline failure ems             = Reply<'T> (false, Unchecked.defaultof<'T>, ems)

let timeIt what n action = 
    // Dry run
    ignore <| action ()

    printfn "Running %s %d times ..." what n
    let sw = Stopwatch ()
    sw.Start ()
    for i in 1..n do
        ignore <| action ()
    sw.Stop ()
    printfn "... took %d ms" sw.ElapsedMilliseconds


let wsTest (ch : char) (_ : int) : bool = 
    match ch with
    | ' '
    | '\t'
    | '\n'
    | '\r'  -> true
    | _     -> false

let qwsTest : Expr<char->int->bool> = 
    <@
        fun ch _ -> 
            match ch with
            | ' '
            | '\t'
            | '\n'
            | '\r'  -> true
            | _     -> false
    @>
[<EntryPoint>]
let main argv = 

    let skipWs1 (cs : UParser.CharStream<unit>) = cs.SkipWhitespaces()
    let skipWs2 (cs : UParser.CharStream<unit>) = 
        let test = UParser.CharTest.Adapt <| wsTest
        cs.Skip test
    let skipWs3 (cs : UParser.CharStream<unit>) = 
        let test = wsTest
        cs.Skip2 test


    let document    = System.String (' ', 10000)
    let n = 400
    let cs          = UParser.CharStream(document, ())

    timeIt "Version1" n <| fun () -> cs.SetPosition 0; skipWs1 cs
    timeIt "Version2" n <| fun () -> cs.SetPosition 0; skipWs2 cs
    timeIt "Version3" n <| fun () -> cs.SetPosition 0; skipWs3 cs

    let qcs = QParser.CharStream<unit> (document, ())
    let skipper = qcs.MakeSkip qwsTest
    timeIt "Version4" n <| fun () -> qcs.SetPosition 0; skipper ()

    0
