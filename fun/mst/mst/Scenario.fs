
// mst - Monadic Scenario Test
namespace mst

open System
open System.Threading

type ScenarioResult = 
    | InfoMessage       of string
    | WarningMessage    of string
    | ErrorMessage      of string
    | ExceptionFailure  of Exception


type StackFrame = 
    {
        Lifted          : bool
        Variables       : Map<string, obj>
        CleanupActions  : (unit -> unit) list
    }
    static member New lifted variables cleanupActions = {Lifted = lifted; Variables = variables; CleanupActions = cleanupActions}
    static member Empty = StackFrame.New false Map.empty []


type ScenarioState = 
    {
        Parameters      : Map<string, obj>
        StackFrames     : StackFrame list
        Results         : ScenarioResult list
    }
    static member New parameters stackFrames results = {Parameters = parameters; StackFrames = stackFrames; Results = results}

type ScenarioRun<'T> = 
    {
        State       : ScenarioState
        Result      : 'T option
    }
    static member New                   state result        = { State = state; Result = result; }
    static member Success               state result        = ScenarioRun<_>.New state (Some result)
    static member SuccessWithInfo       state result info   = ScenarioRun<_>.New {state with Results = (InfoMessage info)::state.Results} (Some result)
    static member SuccessWithWarning    state result warning= ScenarioRun<_>.New {state with Results = (WarningMessage warning)::state.Results} (Some result)
    static member ErrorMessage          state error         = ScenarioRun<_>.New {state with Results = (ErrorMessage error)::state.Results} None
    static member ExceptionFailure      state exc           = ScenarioRun<_>.New {state with Results = (ExceptionFailure exc)::state.Results} None

type Scenario<'T> = 
    {
        Run         : ScenarioState -> ScenarioRun<'T>
    }
    static member New run = {Run = run}

module Scenario =
    let Nop () = ()

    let Return v                    : Scenario<'T> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.Success ss v)
    let Zero ()                     : Scenario<'T> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.Success ss DefaultOf<'T>)

    let ReturnWithInfo    v info    : Scenario<'T> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.SuccessWithInfo       ss v info   )
    let ReturnWithWarning v warning : Scenario<'T> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.SuccessWithWarning    ss v warning)
    let ErrorMessage        error   : Scenario<'T> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.ErrorMessage          ss error    )
    let ExceptionFailure    exc     : Scenario<'T> = Scenario<_>.New <| (fun ss -> ScenarioRun<_>.ExceptionFailure      ss exc      )

    let ReturnFrom (l : Scenario<'T>) : Scenario<'T> = l

    let Yield       = Return
    let YieldFrom   = ReturnFrom

    let Delay (l : unit -> Scenario<'T>) : Scenario<'T> = 
        Scenario<_>.New <| (fun ss ->   
            l().Run ss
            )

    let Pause (ms : int) : Scenario<unit> = 
        Scenario<_>.New <| (fun ss ->   
            Thread.Sleep(ms)
            ScenarioRun<_>.Success ss ()
            )

    let Run (l : Scenario<'T>) : Scenario<'T> = 
        let Cleanup (l : StackFrame list) = 
            for sf in l do
                for cleanupAction in sf.CleanupActions do
                    cleanupAction()
        Scenario<_>.New <| (fun ss -> 
            try
                let length = ss.StackFrames.Length
                let run = l.Run {ss with StackFrames = StackFrame.Empty::ss.StackFrames}  

                let kept,sfs = Slice (length + 1) run.State.StackFrames

                match sfs with
                |   []                      ->  failwith "StackFrame unexpectedly empty"
                |   sf::tail when sf.Lifted ->  run
                |   sf::tail                ->  Cleanup kept
                                                ScenarioRun<_>.New {run.State with StackFrames = tail} run.Result
            with
            |   e   ->  ScenarioRun<_>.ExceptionFailure ss <| e
            )
                
        
    let Bind (l : Scenario<'T>) (r : 'T -> Scenario<'U>) : Scenario<'U> = 
        Scenario<_>.New <| (fun ss ->   
            let lr = l.Run ss 
            match lr.Result with
            |   Some lrr    -> (r lrr).Run lr.State
            |   _           -> ScenarioRun<'U>.New lr.State None
            )    

    let Combine (l : Scenario<unit>) (r : Scenario<'T>) : Scenario<'T> =
        Scenario<_>.New <| (fun ss ->   
            let lr = l.Run ss 
            match lr.Result with
            |   Some _      -> r.Run lr.State
            |   _           -> ScenarioRun<_>.New lr.State None
            )

    let For (s : seq<'T>) (r : 'T -> Scenario<'U>) : Scenario<'U> =
        Scenario<_>.New <| (fun ss ->   
            let mutable state   = ss
            let mutable result  = DefaultOf<_>
            let cont            = ref true
            for l in s |> Seq.filter (fun _ -> !cont) do
                let rs = r l
                let rr = rs.Run state
                state <- rr.State
                match rr.Result with
                |   Some r      -> result <- r
                |   _           -> cont := false
            if !cont then ScenarioRun<_>.Success state result
            else ScenarioRun<_>.New state None
            )

    let While (e : unit -> bool) (r : Scenario<'T>) : Scenario<'T> =
        Scenario<_>.New <| (fun ss ->   
            let mutable state   = ss
            let mutable result  = DefaultOf<_>
            let cont            = ref true
            while !cont && e() do
                let rr = r.Run state
                state <- rr.State
                match rr.Result with
                |   Some r      -> result <- r
                |   _           -> cont := false
            if !cont then ScenarioRun<_>.Success state result
            else ScenarioRun<_>.New state None
            )


    let Optional (l : Scenario<'T>) : Scenario<'T option> = 
        Scenario<_>.New <| (fun ss ->   
            let run = l.Run ss
            match run.Result with
            | Some v    -> ScenarioRun<_>.Success run.State (Some v)
            | _         -> ScenarioRun<_>.Success ss None
            )

    let Ignore (l : Scenario<'T>) : Scenario<'T option> = 
        Scenario<_>.New <| (fun ss ->   
            let run = l.Run ss
            match run.Result with
            | Some v    -> ScenarioRun<_>.Success run.State (Some v)
            | _         -> ScenarioRun<_>.Success run.State None
            )

    let GetParameter k : Scenario<'T> = 
        Scenario<_>.New <| (fun ss ->   
            let pick = ss.Parameters |> Map.tryFind k
            match pick with
            | Some v    -> 
                match v with
                | :? 'T as t    -> ScenarioRun<_>.Success ss t
                | _             -> ScenarioRun<_>.ErrorMessage ss <| "Parameter found but not castable to: " +  typeof<'T>.FullName
            | _         -> ScenarioRun<_>.ErrorMessage ss <| "Parameter not found: " +  k
            )

    let TryGetParameter k : Scenario<'T option> = 
        Scenario<_>.New <| (fun ss ->   
            let pick = ss.Parameters |> Map.tryFind k
            match pick with
            | Some v    -> 
                match v with
                | :? 'T as t    -> ScenarioRun<_>.Success ss (Some t)
                | _             -> ScenarioRun<_>.ErrorMessage ss <| "Parameter found but not castable to: " +  typeof<'T>.FullName
            | _         -> ScenarioRun<_>.Success ss None
            )

    let GetVariable k : Scenario<'T> = 
        Scenario<_>.New <| (fun ss ->   
            let pick = ss.StackFrames |> List.tryPick (fun sf -> sf.Variables |> Map.tryFind k)
            match pick with
            | Some v    -> 
                match v with
                | :? 'T as t    -> ScenarioRun<_>.Success ss t
                | _             -> ScenarioRun<_>.ErrorMessage ss <| "Variable found but not castable to: " +  typeof<'T>.FullName
            | _         -> ScenarioRun<_>.ErrorMessage ss <| "Variable not found: " +  k
            )
    
    let UpdateStackFrame (updater : StackFrame -> StackFrame) : Scenario<unit> =
        Scenario<_>.New <| (fun ss ->   
            match ss.StackFrames with
            |   []          -> failwith "StackFrame unexpectedly empty"
            |   sf::tail    -> let newSf = updater sf
                               let state = {ss with StackFrames = newSf::tail}
                               ScenarioRun<_>.Success state ()      
            )

    let SetVariable k v : Scenario<unit> = 
        UpdateStackFrame (fun sf -> let vs = sf.Variables |> Map.remove k |> Map.add k v
                                    StackFrame.New sf.Lifted vs sf.CleanupActions
                         )
        
    let SetCleanupAction v : Scenario<unit> = 
        UpdateStackFrame (fun sf -> StackFrame.New sf.Lifted sf.Variables (v::sf.CleanupActions))

    let LiftStackFrame: Scenario<unit> = 
        UpdateStackFrame (fun sf -> StackFrame.New true sf.Variables sf.CleanupActions)

    let Raise error : Scenario<'T> = 
        Scenario<_>.New <| (fun ss ->   
            ScenarioRun<_>.ErrorMessage ss error
            )
        
    let Retry (retries : int) (sleepInMilliSeconds : int) (s : Scenario<'T>) : Scenario<'T> = 
        Scenario<_>.New <| (fun ss ->   
            let result = 
                seq {1..retries}
                |> Seq.map (fun i -> 
                    if i > 1 then
                        Thread.Sleep(sleepInMilliSeconds)
                    s.Run ss
                    )
                |> Seq.tryFind (fun r -> r.Result.IsSome)
            match result with
            | Some run  -> run
            | _         -> ScenarioRun<_>.ErrorMessage ss <| "Giving up after: " + retries.ToString()
            )

    let RunScenario (p : Map<string, obj>) (s : Scenario<'T>) : ScenarioRun<'T> = 
        let ss = ScenarioState.New p [] []
        let run = s.Run ss
        run

[<AutoOpen>]
module ScenarioBuilder =

    type ScenarioBuilder() =
        member x.Return(value)                  = Scenario.Return value
        member x.Zero()                         = Scenario.Zero
        member x.ReturnFrom(value)              = Scenario.ReturnFrom value
        member x.Yield(value)                   = Scenario.Yield value
        member x.YieldFrom(value)               = Scenario.YieldFrom value
        member x.Delay(func)                    = Scenario.Delay func
        member x.Run(func)                      = Scenario.Run func
        member x.Bind(func, comp)               = Scenario.Bind func comp
        member x.Combine(expr1, expr2)          = Scenario.Combine expr1 expr2
        member x.For(expr1, expr2)              = Scenario.For expr1 expr2
        member x.While(expr1, expr2)            = Scenario.While expr1 expr2

    let scenario = ScenarioBuilder()

    let inline ( >>= ) l r = Scenario.Bind l r

