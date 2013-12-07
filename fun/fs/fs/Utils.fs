namespace FolderSize

open SharpDX

open System
open System.Diagnostics
open System.Threading
open System.Windows.Forms

[<AutoOpen>]
module Utils =
    
    let GlobalTime =    let sw = new Stopwatch ()
                        sw.Start ()
                        sw

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

                let nextTimeOut = ref <| GlobalTime.ElapsedMilliseconds + timeOut
                while not ct.IsCancellationRequested do
                    let diff = !nextTimeOut - GlobalTime.ElapsedMilliseconds
                    if diff < 1L then
                        ontimeout ()
                        nextTimeOut := GlobalTime.ElapsedMilliseconds + (diff % timeOut) + timeOut
                    let! a = input.TryReceive (int diff)
                    if not ct.IsCancellationRequested then 
                        match a with
                        | Some aa ->    aa()
                        | _       ->    ontimeout()
                                        nextTimeOut := GlobalTime.ElapsedMilliseconds + (diff % timeOut) + timeOut
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

    let inline ( <?+?> ) l r = CombineDisposable l r

module ListEx =
    
    let rec any (t : 'T->bool) (l : 'T list) = (List.tryFind t l).IsSome
            
