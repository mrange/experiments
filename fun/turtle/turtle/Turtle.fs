namespace TurtlePower


open SharpDX
open System

module Turtle = 

    let deg2rad = float32 Math.PI/180.F
    let rad2deg = 1.F / deg2rad

    type Line = Vector2*Vector2
    
    type DrawLine = float->Vector2->Vector2->unit

    type State =
        {
            Width       : float
            Position    : Vector2
            Direction   : Vector2
            DrawLine    : DrawLine
        }
        static member New w p d dl = 
            let dd : Vector2 = d 
            ignore <| dd.Normalize()
            {Width = w; Position = p; Direction = dd; DrawLine = dl;}

    type Movement<'T> =
        {
            Value       : 'T
            State       : State
        }
        static member New v s = {Value = v; State = s;}

    type Turtle<'T> = State -> Movement<'T>

    let Return v                        : Turtle<'T>    = (fun s ->   Movement<_>.New v s)
    let Zero ()                         : Turtle<'T>    = (fun s ->   Movement<_>.New Unchecked.defaultof<_> s)
    let ReturnFrom (t : Turtle<'T>)     : Turtle<'T>    = t
    let Yield                                           = Return
    let YieldFrom                                       = ReturnFrom
    let Delay (tg : unit -> Turtle<'T>) : Turtle<'T>    = (fun s -> tg () s)
    let Run (t : Turtle<'T>)            : Turtle<'T>    = (fun s -> t s)

    let RunAndReturn (t : Turtle<'T>)   : Turtle<'T>    = 
        (fun s ->   let m = t s
                    Movement<_>.New m.Value s
        )

    let Bind (l : Turtle<'T>) (r : 'T -> Turtle<'U>)    : Turtle<'U> = 
        (fun s ->   
            let m = l s
            (r m.Value) m.State
        )

    let Combine (l : Turtle<unit>) (r : Turtle<_>)      : Turtle<_> =
        (fun s ->   
            let m = l s
            r m.State
        )

    let For (seq : seq<'T>) (r : 'T -> Turtle<_>) : Turtle<_> =
        (fun s ->   
            let mutable state   = s
            let mutable result  = Unchecked.defaultof<_>
            for v in seq do
                let mm = r v state 
                state <- mm.State
                result <- mm.Value
            result
        )

    let While (e : unit -> bool) (r : Turtle<_>) : Turtle<_> =
        (fun s ->   
            let mutable state   = s
            let mutable result  = Unchecked.defaultof<_>
            while e() do
                let mm = r state
                state <- mm.State
                result <- mm.Value
            result
        )

    let Width w : Turtle<unit>= 
        (fun s -> 
            let ss = State.New w s.Position s.Direction s.DrawLine
            Movement<_>.New () ss
        )

    let Forward v : Turtle<unit>= 
        (fun s -> 
            let p = s.Position + v*s.Direction 
            let ss = State.New s.Width p s.Direction s.DrawLine
            ss.DrawLine s.Width s.Position p
            Movement<_>.New () ss
        )

    let Turn a : Turtle<unit>= 
        (fun s -> 
            let r = Matrix3x2.Rotation(deg2rad * a)
            let d = Matrix3x2.TransformPoint(r, s.Direction)
            let ss = State.New s.Width s.Position d s.DrawLine
            Movement<_>.New () ss
        )


    let Execute (w : float) (p : Vector2) (d : Vector2) (dl : DrawLine) (t : Turtle<'T>) =
        let s = State.New w p d dl
        t s

[<AutoOpen>]
module TurtleBuilder =

    type TurtleBuilder() =
        member x.Return(value)                  = Turtle.Return value
        member x.Zero()                         = Turtle.Zero
        member x.ReturnFrom(value)              = Turtle.ReturnFrom value
        member x.Yield(value)                   = Turtle.Yield value
        member x.YieldFrom(value)               = Turtle.YieldFrom value
        member x.Delay(func)                    = Turtle.Delay func
        member x.Run(func)                      = Turtle.Run func
        member x.Bind(func, comp)               = Turtle.Bind func comp
        member x.Combine(expr1, expr2)          = Turtle.Combine expr1 expr2
        member x.For(expr1, expr2)              = Turtle.For expr1 expr2
        member x.While(expr1, expr2)            = Turtle.While expr1 expr2

    let turtle = TurtleBuilder()
