
// mst - Monadic Scenario Test
namespace mst


type ScenarioMessage = 
    {
        Message   : string
    }
    static member New message = {Message = message}

type ScenarioFailure = 
    {
        Error   : string
    }
    static member New error = {Error = error}

type ScenarioResult = 
    | Message       of ScenarioMessage
    | Failure       of ScenarioFailure

type ScenarioState = 
    {
        Parameters      : Map<string, obj>
        State           : Map<string, obj>
        Results         : ScenarioResult list
        CleanupActions  : (unit -> unit) list
    }
    static member New parameters state results cleanupActions = {Parameters = parameters; State = state; Results = results; CleanupActions = cleanupActions}

type ScenarioRun<'T> = 
    {
        State       : ScenarioState
        Result      : 'T option
    }
    static member New state result      = {State = state; Result = result; }
    static member Success state result  = ScenarioRun<_>.New state (Some result)
    static member Failure state error   = ScenarioRun<_>.New {state with Results = (Failure (ScenarioFailure.New error))::state.Results} None
    static member Message state msg     = ScenarioRun<_>.New {state with Results = (Message (ScenarioMessage.New msg))::state.Results} None

type Scenario<'T> = 
    {
        Run         : ScenarioState -> ScenarioRun<'T>
    }
    static member New run = {Run = run}

module Scenario =
    let Nop () = ()

    let Return v        : Scenario<_> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.Success ss v)
    let Zero ()         : Scenario<_> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.Success ss Unchecked.defaultof<_>)

    let Message msg     : Scenario<_> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.Message ss msg)
    let Failure error   : Scenario<_> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.Failure ss error)

    let ReturnFrom (l : Scenario<_>) : Scenario<_> = l

    let Yield       = Return
    let YieldFrom   = ReturnFrom

    let Delay (l : unit -> Scenario<_>) : Scenario<_> = l()


    let Bind (l : Scenario<'T>) (r : 'T -> Scenario<'U>) : Scenario<'U> = 
        Scenario<_>.New <| (fun ss ->   
            let lr = l.Run ss 
            match lr.Result with
            |   Some lrr    -> (r lrr).Run lr.State
            |   _           -> ScenarioRun<'U>.New lr.State None
            )    

    let Combine (l : Scenario<unit>) (r : Scenario<_>) : Scenario<_> =
        Scenario<_>.New <| (fun ss ->   
            let lr = l.Run ss 
            match lr.Result with
            |   Some _      -> r.Run lr.State
            |   _           -> ScenarioRun<_>.New lr.State None
            )

    let For (s : seq<'T>) (r : 'T -> Scenario<_>) : Scenario<_> =
        Scenario<_>.New <| (fun ss ->   
            let mutable state   = ss
            let mutable result  = Unchecked.defaultof<_>
            let cont            = ref true
            for l in s |> Seq.filter (fun _ -> !cont) do
                let rs = r l
                let rr = rs.Run state
                state <- rr.State
                match rr.Result with
                |   Some r      -> result <- r
                |   _           -> cont := false
            if !cont then result
            else ScenarioRun<_>.New state None
            )

    let While (e : unit -> bool) (r : Scenario<_>) : Scenario<_> =
        Scenario<_>.New <| (fun ss ->   
            let mutable state   = ss
            let mutable result  = Unchecked.defaultof<_>
            let cont            = ref true
            while !cont && e() do
                let rr = r.Run state
                state <- rr.State
                match rr.Result with
                |   Some r      -> result <- r
                |   _           -> cont := false
            if !cont then result
            else ScenarioRun<_>.New state None
            )


    let GetState k : Scenario<'T> = 
        Scenario<_>.New <| (fun ss ->   
            let f = ss.State |> Map.tryFind k
            match f with
            | Some e    -> 
                match e with
                | :? 'T as t    -> ScenarioRun<_>.Success ss t
                | _             -> ScenarioRun<_>.Failure ss ("State found but not castable to: " +  typeof<'T>.FullName)
            | _         -> ScenarioRun<_>.Failure ss ("State not found: " +  k)
            )

    let SetState k v : Scenario<_> = 
        Scenario<_>.New <| (fun ss ->   
            let state = {ss with State = ss.State |> Map.remove k |> Map.add k v}
            ScenarioRun<_>.Success state () 
            )
        
    let Raise error : Scenario<'T> = 
        Scenario<_>.New <| (fun ss ->   
            ScenarioRun<_>.Failure ss error
            )
        
    let ClearState k : Scenario<_> = 
        Scenario<_>.New <| (fun ss ->   
            let state = {ss with State = ss.State |> Map.remove k}
            ScenarioRun<_>.Success state () 
            )
        
    let SetCleanupAction v : Scenario<_> = 
        Scenario<_>.New <| (fun ss ->   
            let state = {ss with CleanupActions = v::ss.CleanupActions}
            ScenarioRun<_>.Success state () 
            )

[<AutoOpen>]
module ScenarioBuilder =

    type ScenarioBuilder() =
        member x.Return(value)                  = Scenario.Return value
        member x.Zero()                         = Scenario.Zero
        member x.ReturnFrom(value)              = Scenario.ReturnFrom value
        member x.Yield(value)                   = Scenario.Yield value
        member x.YieldFrom(value)               = Scenario.YieldFrom value
        member x.Delay(func)                    = Scenario.Delay func
        member x.Bind(func, comp)               = Scenario.Bind func comp
        member x.Combine(expr1, expr2)          = Scenario.Combine expr1 expr2
        member x.For(expr1, expr2)              = Scenario.For expr1 expr2
        member x.While(expr1, expr2)            = Scenario.While expr1 expr2

    let scenario = ScenarioBuilder()

