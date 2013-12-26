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
    let inline Natural (v : 'T when 'T : comparison and 'T : struct) =  if v < Zero then Zero else v

    let Log             (i : string)= printfn "Information : %s" i
    let LogWarning      (w : string)= printfn "Warning     : %s" w
    let LogError        (e : string)= printfn "Error       : %s" e
    let LogException    (e : exn)   = printfn "Exception   : %s" e.Message

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

    let inline ( <*> ) (l : Matrix3x2) (r : Matrix3x2) = Matrix3x2.Multiply(l,r)

    let TryDispose (d : IDisposable) = 
        try
            if d <> null then d.Dispose ()
        with
        | e -> printfn "Caught exception: %A" e

    let TryRun (a : unit -> unit) = 
        try
            a()
        with
        | e -> printfn "Caught exception: %A" e


    let Normalize (v : Vector2) = v.Normalize()
                                  v
    type Disposable(action : unit->unit) = 
        interface IDisposable with
            member x.Dispose() = TryRun action

    let OnExit a : IDisposable = upcast new Disposable(a)

    let AsNullable v = Nullable<_>(v)

    let CompareAndExchange<'T when 'T : not struct> (f : 'T -> 'T) (v : 'T ref) = 
        while let v' = !v in not (Object.ReferenceEquals (Interlocked.CompareExchange (v, f v', v'), v')) do
            ()

    let DispatchAction (c : Control) (a : unit->unit) = 
        let ac = Action a
        ignore <| c.BeginInvoke(ac)

    let DefaultTo (o : 'T option) (d : 'T) = 
        match o with
        | Some v    -> v
        | _         -> d

    let CastTo<'T> (o : obj) (d : 'T) = 
        match o with
        | :? 'T as i    -> i
        | _             -> d
        
    let As<'T> (o : obj) = 
        match o with
        | :? 'T as i    -> Some i
        | _             -> None
        
    let Is<'T> (o : obj) = 
        match o with
        | :? 'T as i    -> true
        | _             -> false
        

    let CombineDisposable (l : #IDisposable) (r : #IDisposable) = 
        OnExit <| fun () -> 
                    TryDispose l
                    r.Dispose ()

    type ActionProcessor (post:(unit->unit)->unit, dispose:unit->unit) = 
        member x.Post a = post a
        interface IDisposable with
            member this.Dispose () = TryRun dispose

    let ActionProcessorWithTimeOut (tp : ThreadPriority option) (timeOut:int64) (ontimeout : unit->unit)=
        if timeOut < 1L then failwith "Timeout must be greater than 0"

        let cts = new CancellationTokenSource ()
        let ct = cts.Token
        let processor (input : MailboxProcessor<unit->unit>) = 
            async {
                match tp with
                | Some tp   -> Thread.CurrentThread.Priority <- tp
                | _         -> ()

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
                        | _       ->    ontimeout()
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

    let ActionProcessor (tp : ThreadPriority option) = 
        let cts = new CancellationTokenSource ()
        let ct = cts.Token
        let processor (input : MailboxProcessor<unit->unit>) = 
            async {
                match tp with
                | Some tp   -> Thread.CurrentThread.Priority <- tp
                | _         -> ()

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

    let inline ( <??> ) o d = DefaultTo o d
    let inline ( <???> ) o d = CastTo o d

    let inline ( <+++> ) l r = CombineDisposable l r

    type Type with 
        member x.Ancestors () = 
            let t = ref x
            seq {
                while !t <> null do
                    yield !t
                    t := (!t).BaseType
            }

    type RectangleF with
        member l.Union (r : RectangleF) =
            if l.IsEmpty then r
            elif r.IsEmpty then l
            else 
                let left    = min l.Left r.Left
                let top     = min l.Top r.Top
                let right   = max l.Right r.Right
                let bottom  = max l.Bottom r.Bottom
                RectangleF (left, top, right - left, bottom - top)

    type Object with
        member x.CastTo (defaultValue : 'T) = x <???> defaultValue
        member x.As<'T> () = As<'T> x
        member x.Is<'T> () = Is<'T> x

    type IDictionary<'TKey, 'TValue> with
        member x.Lookup (k : 'TKey) (dv : 'TValue) =  
                    let v = ref Unchecked.defaultof<'TValue>
                    if x.TryGetValue(k, v) then !v
                    else dv

        member x.Find (k : 'TKey) : 'TValue option =  
                    let v = ref Unchecked.defaultof<'TValue>
                    if x.TryGetValue(k, v) then Some !v
                    else None
                                                

module ListEx =
    
    let rec any (t : 'T->bool) (l : 'T list) = (List.tryFind t l).IsSome
            
    let foldMap (f : 'U -> 'T -> 'U*'V) (s : 'U) (l : 'T list) : 'V list = 
        let state = ref s
        [ 
            for v in l do
                let ns,nv = f !state v
                state := ns
                yield nv
        ]
            


