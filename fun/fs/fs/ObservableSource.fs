namespace FolderSize

open System
open System.Diagnostics
open System.Threading

type IObservableSource<'T> = 
    inherit IObservable<'T>

    abstract member Start       : unit -> unit
    abstract member Next        : 'T -> unit 
    abstract member Completed   : unit -> unit
    abstract member Error       : Exception -> unit

type ObservableSource<'TPayload, 'T>(onStart : IObservableSource<'T> -> 'TPayload, ?onCompleted : 'TPayload -> unit, ?onError : Exception -> unit) =

    [<Literal>]
    let Idle        = 0
    [<Literal>]
    let Running     = 1
    [<Literal>]
    let Finished    = 2

    let state           = ref Idle
    let mutable payload = None
    let key             = ref 0
    let subscriptions   : Map<int, IObserver<'T>> ref = ref Map.empty 

    let oncompleted =   match onCompleted with
                        | Some v    -> v
                        | _         -> fun _ -> ()

    let onerror =   match onError with
                    | Some v    -> v
                    | _         -> fun e -> Debug.Fail <| sprintf "ObservableSource caught exception: %A" e

    let isrunning () = !key = Running

    member this.Start () =  payload <- Some <| onStart this
                            state := Running

    member this.Next v =
        if isrunning () then
            let subs = !subscriptions
            for kv in subs do
                try
                    kv.Value.OnNext v
                with
                | e ->  onerror e

    member this.Completed () =
        let f = Interlocked.Exchange (state, Finished)
        if f = Running then
            try
                let subs = Interlocked.Exchange (subscriptions, Map.empty)
                for kv in subs do
                    try
                        kv.Value.OnCompleted ()
                    with
                    | e ->  onerror e
            finally
                oncompleted payload.Value   // In order to enter running state payload has to be initialized
            

    member this.Error err =
        if isrunning () then
            let subs = !subscriptions
            for kv in subs do
                try
                    kv.Value.OnError err
                with
                | e ->  onerror e

    interface IObservableSource<'T> with
        member this.Start ()        = this.Start ()
        member this.Next v          = this.Next v
        member this.Completed ()    = this.Completed ()
        member this.Error e         = this.Error e
        member this.Subscribe obs   = 
            let k = Interlocked.Increment key
            
            CompareAndExchange (fun m -> Map.add k obs m) subscriptions

            OnExit <| fun () -> CompareAndExchange (fun m -> Map.remove k m) subscriptions

    interface IDisposable with
        member this.Dispose () = TryRun this.Completed


