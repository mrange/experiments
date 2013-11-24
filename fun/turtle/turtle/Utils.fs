namespace TurtlePower

open SharpDX

open System

[<AutoOpen>]
module Utils =
    
    let Deg2Rad = float32 Math.PI/180.F
    let Rad2Deg = 1.F / Deg2Rad

    let DefaultOf<'T> = Unchecked.defaultof<'T>

    let NewVector2 x y = Vector2(x,y)

    let inline ( <*> ) (l : Matrix3x2) (r : Matrix3x2) = Matrix3x2.Multiply(l,r)

    let TryRun (a : unit -> unit) = 
        try
            a()
        with
        | e -> printfn "Caught exception: %A" e
