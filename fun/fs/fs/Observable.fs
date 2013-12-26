namespace FolderSize

open System
open System.Diagnostics
open System.Threading
open System.Windows.Forms


module ObservableEx = 
    
    [<NoEquality>]
    [<NoComparison>]
    type Observer<'T> =
        {
            OnNext      : 'T -> unit
            OnCompleted : unit -> unit
            OnError     : exn -> unit
        }

        interface IObserver<'T> with
            member this.OnNext t =
                this.OnNext t

            member this.OnCompleted() =
                this.OnCompleted()

            member this.OnError err =
                this.OnError err

        static member New onNext onComplete onError =
            {
                OnNext      = onNext
                OnCompleted = onComplete
                OnError     = onError
            } :> IObserver<'T>

    [<NoEquality>]
    [<NoComparison>]
    type Observable<'T> =
        {
            OnSubscribe : IObserver<'T> -> IDisposable
        }

        interface IObservable<'T> with

            member this.Subscribe o =
                this.OnSubscribe o

        member this.Subscribe (onNext: 'T -> unit) (onComplete: unit -> unit) (onError: exn -> unit) =
            let observer = Observer<_>.New onNext onComplete onError
            this.OnSubscribe observer

        static member New onSubscribe = { OnSubscribe = onSubscribe } :> IObservable<'T>

    let terminator onNext onComplete onError (o : IObservable<'T>) = 
        let obs = Observer<_>.New   onNext
                                    onComplete
                                    onError
        o.Subscribe obs

    let terminator_Next onNext (o : IObservable<'T>) = 
        terminator onNext (fun () -> ()) LogException o

    let deref (o : IObservable<'T option>) : IObservable<'T> =
        Observable<_>.New <| 
            fun observer -> 
                let obs = Observer<_>.New   (fun v  -> match v with 
                                                       | Some vv -> observer.OnNext vv 
                                                       | None    -> ()
                                            )
                                            (fun () -> observer.OnCompleted ())
                                            (fun exn-> observer.OnError exn)

                o.Subscribe obs

    let foldMap (f : 'U -> 'T -> 'U*'V) (s : 'U) (o : IObservable<'T>) : IObservable<'V> = 
        Observable<_>.New <| 
            fun observer -> 
                let state = ref s

                let obs = Observer<_>.New   (fun v  -> let s,vv = f !state v
                                                       state := s
                                                       observer.OnNext vv
                                            )
                                            (fun () -> observer.OnCompleted ())
                                            (fun exn-> observer.OnError exn)

                o.Subscribe obs

    let asyncFold (tp : ThreadPriority option) (timeout : int64) (f : 'U -> 'T -> 'U) (s : 'U) (o : IObservable<'T>) = 
        Observable<_>.New <| 
            fun observer -> 
                let state = ref s

                let actionProcessor = ActionProcessorWithTimeOut tp timeout (fun () -> observer.OnNext !state)

                let obs = Observer<_>.New   (fun v  -> actionProcessor.Post <| fun () -> state := f !state v)
                                            (fun () -> actionProcessor.Post <| fun () -> observer.OnNext !state; observer.OnCompleted ())
                                            (fun exn-> actionProcessor.Post <| fun () -> observer.OnError exn)

                actionProcessor.Combine <| o.Subscribe obs

    let async (tp : ThreadPriority option) (o : IObservable<'T>) = 
        Observable<_>.New <| 
            fun observer -> 
                let actionProcessor = ActionProcessor tp

                let obs = Observer<_>.New   (fun v  -> actionProcessor.Post <| fun () -> observer.OnNext v)
                                            (fun () -> actionProcessor.Post <| fun () -> observer.OnCompleted ())
                                            (fun exn-> actionProcessor.Post <| fun () -> observer.OnError exn)

                actionProcessor.Combine <| o.Subscribe obs

    let asyncTerminator (tp : ThreadPriority option) onNext onComplete onError (o : IObservable<'T>) = 
        let actionProcessor = ActionProcessor tp

        let obs = Observer<_>.New   (fun v  -> actionProcessor.Post <| fun () -> onNext v)
                                    (fun () -> actionProcessor.Post <| fun () -> onComplete ())
                                    (fun exn-> actionProcessor.Post <| fun () -> onError exn)

        actionProcessor.Combine <| o.Subscribe obs

    let dispatch (c : Control) (o : IObservable<'T>) : IObservable<'T> = 
        let dispatcher = DispatchAction c
        Observable<_>.New <| 
            fun observer -> 
                let obs = Observer<_>.New   (fun v  -> dispatcher <| fun () -> observer.OnNext v)
                                            (fun () -> dispatcher <| fun () -> observer.OnCompleted ())
                                            (fun exn-> dispatcher <| fun () -> observer.OnError exn)
                o.Subscribe obs
                                        

type IObservableSource = 
    inherit IDisposable

    abstract member Start       : unit -> unit

type IObservableSource<'T> = 
    inherit IObservable<'T>
    inherit IObservableSource

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
                        | None      -> fun _ -> ()

    let onerror =   match onError with
                    | Some v    -> v
                    | None      -> fun e -> Debug.Fail <| sprintf "ObservableSource caught exception: %A" e

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

    interface IObservableSource with
        member this.Start ()        = this.Start ()

    interface IObservableSource<'T> with
        member this.Next v          = this.Next v
        member this.Completed ()    = this.Completed ()
        member this.Error e         = this.Error e
        member this.Subscribe obs   = 
            let k = Interlocked.Increment key
            
            CompareAndExchange (fun m -> Map.add k obs m) subscriptions

            OnExit <| fun () -> CompareAndExchange (fun m -> Map.remove k m) subscriptions

    interface IDisposable with
        member this.Dispose () = TryRun this.Completed


