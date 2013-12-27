namespace FolderSize

open SharpDX

open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Windows.Forms

[<AutoOpen>]
module Utils =
    
    let inline Zero<'T when 'T : struct> = Unchecked.defaultof<'T>
    let inline Natural (comparable : 'T when 'T : comparison and 'T : struct) =  if comparable < Zero then Zero else comparable

    let Log             (message  : string)= printfn "Information : %s" message
    let LogWarning      (message  : string)= printfn "Warning     : %s" message
    let LogError        (message  : string)= printfn "Error       : %s" message
    let LogException    (exn      : exn)   = printfn "Exception   : %s" exn.Message

    let GlobalClock =   let sw = new Stopwatch ()
                        sw.Start ()
                        sw

    let CurrentTime () : Time = 
        (float32 GlobalClock.ElapsedMilliseconds) / 1000.F


    let CurrentState () : ApplicationState = 
        ApplicationState.New (CurrentTime ()) (MouseState.New Set.empty (Vector2 ()))
    
    let Deg2Rad = float32 Math.PI/180.F
    let Rad2Deg = 1.F / Deg2Rad

    let DefaultOf<'T> = Unchecked.defaultof<'T>

    let NewVector2 x y = Vector2(x,y)

    let TryDispose (disposabe : IDisposable) = 
        try
            if disposabe <> null then disposabe.Dispose ()
        with
        | exn -> printfn "Caught exception: %A" exn

    let TryRun (action : unit -> unit) = 
        try
            action()
        with
        | exn -> printfn "Caught exception: %A" exn


    let Normalize (v : Vector2) = v.Normalize()
                                  v

    type Disposable(action : unit->unit) = 
        interface IDisposable with
            member x.Dispose() = TryRun action

    let OnExit action : IDisposable = upcast new Disposable(action)

    let AsNullable value = Nullable<_>(value)

    let CompareAndExchange<'T when 'T : not struct> (f : 'T -> 'T) (valueReference : 'T ref) = 
        while let value = !valueReference in not (Object.ReferenceEquals (Interlocked.CompareExchange (valueReference, f value, value), value)) do
            ()

    let DispatchAction (control : Control) (action : unit->unit) = 
        let a = Action action
        ignore <| control.BeginInvoke(a)

    let DefaultTo (optional : 'T option) (defaultValue : 'T) = 
        match optional with
        | Some v    -> v
        | _         -> defaultValue

    let CastTo<'T> (o : obj) (defaultValue : 'T) = 
        match o with
        | :? 'T as v    -> v
        | _             -> defaultValue
        
    let As<'T> (o : obj) = 
        match o with
        | :? 'T as v    -> Some v
        | _             -> None
        
    let Is<'T> (o : obj) = 
        match o with
        | :? 'T         -> true
        | _             -> false
        

    let CombineDisposable (l : #IDisposable) (r : #IDisposable) = 
        OnExit <| fun () -> 
                    TryDispose l
                    r.Dispose ()

    type ActionProcessor (post:(unit->unit)->unit, dispose:unit->unit) = 
        member x.Post a = post a
        interface IDisposable with
            member this.Dispose () = TryRun dispose

    let ActionProcessorWithTimeOut (threadPriority : ThreadPriority option) (timeOut:int64) (ontimeout : unit->unit)=
        if timeOut < 1L then failwith "Timeout must be greater than 0"

        let cts = new CancellationTokenSource ()
        let ct = cts.Token
        let processor (input : MailboxProcessor<unit->unit>) = 
            async {
                match threadPriority with
                | Some tp   -> Thread.CurrentThread.Priority <- tp
                | None      -> ()

                let nextTimeOut = ref <| GlobalClock.ElapsedMilliseconds + timeOut
                while not ct.IsCancellationRequested do
                    let diff = !nextTimeOut - GlobalClock.ElapsedMilliseconds
                    if diff < 1L then
                        ontimeout ()
                        nextTimeOut := GlobalClock.ElapsedMilliseconds + (diff % timeOut) + timeOut
                    let! a = input.TryReceive (int diff)
                    if not ct.IsCancellationRequested then 
                        match a with
                        | Some aa ->    aa()
                        | None    ->    ontimeout()
                                        nextTimeOut := GlobalClock.ElapsedMilliseconds + (diff % timeOut) + timeOut
            }
        let mb = MailboxProcessor<unit->unit>.Start (processor,ct)
        new ActionProcessor (
            mb.Post, 
            fun () -> 
                mb.Post <| fun () -> ()
                TryDispose cts
                TryDispose mb 
                )

    let ActionProcessor (threadPriority : ThreadPriority option) = 
        let cts = new CancellationTokenSource ()
        let ct = cts.Token
        let processor (input : MailboxProcessor<unit->unit>) = 
            async {
                match threadPriority with
                | Some tp   -> Thread.CurrentThread.Priority <- tp
                | None      -> ()

                while not ct.IsCancellationRequested do
                    let! a = input.Receive ()
                    if not ct.IsCancellationRequested then 
                        a()
            }
        let mb = MailboxProcessor<unit->unit>.Start (processor,ct)
        new ActionProcessor (
            mb.Post, 
            fun () -> 
                mb.Post <| fun () -> ()
                TryDispose cts
                TryDispose mb 
                )

    // TODO: Scrap these and replace with extension methods?
    // Extension methods seems easier to maintain since function application has high precence leads to less parantese

    let inline ( <??> ) optional defaultValue = DefaultTo optional defaultValue
    let inline ( <???> ) o defaultValue = CastTo o defaultValue

    type IDisposable with 
        member x.Combine (o : IDisposable) = CombineDisposable x o            

    type Matrix3x2 with 
        member x.Multiply (o : Matrix3x2) = Matrix3x2.Multiply(x,o) 

    type Type with 
        member x.Ancestors () = 
            let t = ref x
            seq {
                while !t <> null do
                    yield !t
                    t := (!t).BaseType
            }

    type RectangleF with
        member x.Union (other : RectangleF) =
            if x.IsEmpty then other
            elif other.IsEmpty then x
            else 
                let left    = min x.Left    other.Left
                let top     = min x.Top     other.Top
                let right   = max x.Right   other.Right
                let bottom  = max x.Bottom  other.Bottom
                RectangleF (left, top, right - left, bottom - top)

    type Object with
        member x.CastTo (defaultValue : 'T) = x <???> defaultValue
        member x.As<'T> () = As<'T> x
        member x.Is<'T> () = Is<'T> x

    type IDictionary<'TKey, 'TValue> with
        member x.Lookup (key : 'TKey) (defaultValue : 'TValue) =  
                    let v = ref Unchecked.defaultof<'TValue>
                    if x.TryGetValue(key, v) then !v
                    else defaultValue

        member x.Find (key : 'TKey) : 'TValue option =  
                    let v = ref Unchecked.defaultof<'TValue>
                    if x.TryGetValue(key, v) then Some !v
                    else None
                                                

module List =
    
    let rec any (test : 'T->bool) (l : 'T list) = (List.tryFind test l).IsSome
            
    let foldMap (foldAndMap : 'U -> 'T -> 'U*'V) (state : 'U) (l : 'T list) : 'V list = 
        let state = ref state
        [ 
            for v in l do
                let nextState,nextValue = foldAndMap !state v
                state := nextState
                yield nextValue
        ]
            
module Array =
    
    let rec any (test : 'T->bool) (l : 'T array) = (Array.tryFind test l).IsSome
            
    let foldMap (foldAndMap : 'U -> 'T -> 'U*'V) (state : 'U) (l : 'T array) : 'V list = 
        let state = ref state
        [ 
            for v in l do
                let nextState,nextValue = foldAndMap !state v
                state := nextState
                yield nextValue
        ]
            


