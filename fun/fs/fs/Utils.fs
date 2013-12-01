namespace FolderSize

open SharpDX

open System
open System.Threading

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

    let CompareAndExchange<'T when 'T : not struct> (f : 'T -> 'T) (v : 'T ref) = 
        while let v' = !v in not (Object.ReferenceEquals (Interlocked.CompareExchange (v, f v', v'), v')) do
            ()
