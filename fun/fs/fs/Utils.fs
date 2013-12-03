namespace FolderSize

open SharpDX

open System
open System.Threading
open System.Windows.Forms

[<AutoOpen>]
module Utils =
    
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

    let inline ( <??> ) o d = DefaultTo o d