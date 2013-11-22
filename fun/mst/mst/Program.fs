open mst

open System.Windows
open System.Windows.Media
open System.Windows.Automation

open mst.lowlevel

module LSystem =
    type Alphabet =
        | DoNothing     // X
        | Forward       // F
        | TurnLeft      // -
        | TurnRight     // +
        | Store         // [
        | Restore       // ]

    let Angle = 25.

    let Initial = [DoNothing]

    let Rule (a : Alphabet) = 
        match a with
                     // F       -        [     [     X         ]       +         X         ]       +         F       [     +         F       X         ]       -        X 
        | DoNothing -> [Forward;TurnLeft;Store;Store;DoNothing;Restore;TurnRight;DoNothing;Restore;TurnRight;Forward;Store;TurnRight;Forward;DoNothing;Restore;TurnLeft;DoNothing]
                     // F       F
        | Forward   -> [Forward;Forward]
        | _         -> [a]

    let rec Generate =  function 
                        | 0 ->  Initial         
                        | n ->  let previous = Generate (n - 1)
                                previous |> List.collect Rule

module Turtle = 

    type Line = Vector*Vector
    
    type DrawLine = float->Vector->Vector->unit

    type State =
        {
            Width       : float
            Position    : Vector
            Direction   : Vector
            DrawLine    : DrawLine
        }
        static member New w p d dl = 
            let dd : Vector = d 
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
    let Run (t : Turtle<'T>)            : Turtle<'T>    = 
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
            let r = Matrix.Identity
            r.Rotate(a)
            let d = s.Direction
            let d = r.Transform(d)
            let ss = State.New s.Width s.Position d s.DrawLine
            Movement<_>.New () ss
        )


    let Execute (w : float) (p : Vector) (d : Vector) (dl : DrawLine) (t : Turtle<'T>) =
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

//    let inline ( >>= ) l r = Scenario.Bind l r

module TurtleFractal =
    
    let Left = -25.
    let Right = 25.

    // [Forward;TurnLeft;Store;Store;DoNothing;Restore;TurnRight;DoNothing;Restore;TurnRight;Forward;Store;TurnRight;Forward;DoNothing;Restore;TurnLeft;DoNothing]

    let rec GenerateSubTree n v = 
        turtle {
            do! Generate (n - 1) (v / 2.)
        }
    and GenerateRotatedSubTree n v a = 
        turtle {
            do! Turtle.Turn a
            do! GenerateSubTree n v
        }
    and GenerateBranch n v = 
        turtle {
            do! Turtle.Turn Right
            do! Turtle.Forward v
            do! GenerateSubTree n v
        }
    and Generate n v = 
        turtle {
            if n <= 0 then
                return ()
            else
                do! Turtle.Width <| (1.5 * float n)
                do! Turtle.Forward v
                do! Turtle.Turn Left
                do! GenerateSubTree n v
                do! GenerateRotatedSubTree n v Right
                do! Turtle.Turn Right
                do! Turtle.Forward v
                do! GenerateBranch n v
                do! Turtle.Turn Left
                do! GenerateSubTree n v
        }
     
type LineSize =
    |   Px1
    |   Px3
    |   Px5
    |   Px8

[<EntryPoint>]
let main argv = 


    let StartMSPaint = UIScenario.StartWindowedProcess "mspaint.exe"
    let StartNotepad = UIScenario.StartWindowedProcess "notepad.exe"

    let WaitForPopup = Scenario.Retry 5 100

    let SelectTool (toolName : string) = UIScenario.Invoke <| ByName toolName

    let GetDrawingBounds = UIScenario.GetBounds <| ByClass "MSPaintView" 

    let offset = 32.

    let Draw (bounds : Rect) (f : Vector) (t : Vector) = 
        let d = t - f
        ignore <| d.Normalize()
        let o = offset * d + t
        UIScenario.DoMouseGesture   [
                                        LeftClickAndHold<| Point(bounds.Left + f.X, bounds.Top + f.Y)
                                        ReleaseLeft     <| Point(bounds.Left + t.X, bounds.Top + t.Y)
                                        LeftClick       <| Point(bounds.Left + o.X, bounds.Top + o.Y)
                                    ]

    let DrawSomething (toolName : string) (cx : float) (cy : float) (w : float) (h : float) = 
        scenario {
            do! SelectTool toolName

            let! bounds = GetDrawingBounds

            let f = Vector(cx,cy)
            let t = Vector(cx + w,cy + h)

            do! Draw bounds f t
        }

    let SelectSize (lz : LineSize) : Scenario<unit> = 
        scenario {
            let toolName = 
                match lz with
                |   Px1 -> "1px"
                |   Px3 -> "3px"
                |   Px5 -> "5px"
                |   Px8 -> "8px"

            do! UIScenario.Invoke <| ByName "Size"
            do! UIScenario.Invoke <| ByName toolName

            return ()
        }

    let DrawLine        = DrawSomething "Line"
    let DrawOval        = DrawSomething "Oval"
    let DrawRectangle   = DrawSomething "Rectangle"

    let ConfirmSaveAs : Scenario<unit> = 
        scenario {
            do! WaitForPopup (UIScenario.FocusElement <| ByName "Confirm Save As")
            do! UIScenario.SendChar 'y' Modifier.LeftAlt
        }

    let SaveFile fileName : Scenario<unit> = 
        scenario {
            do! UIScenario.SendChar 'f' Modifier.LeftAlt
            do! UIScenario.SendChar 'a' Modifier.None

            do! WaitForPopup (UIScenario.SetCurrentElement <| ByName "Save As")
            do! UIScenario.FocusElement <| ById "1001"

            do! UIScenario.SendText fileName

            do! UIScenario.SendChar 's' Modifier.LeftAlt

            let! x = Scenario.Optional ConfirmSaveAs

            return ()
        }

    let myScenario1 = scenario {
        do! StartMSPaint

        do! DrawOval        100. 100. 200. 200.
        do! DrawRectangle   200. 200. 100. 100.
        do! DrawLine        300. 300. 200. 200.

        do! SaveFile "tester.png"

        do! Scenario.Pause  2000
        }

    let myScenario2 = scenario {
        do! StartNotepad

        do! Scenario.Pause  500

        do! UIScenario.SendText "Good game!"

        do! UIScenario.SendChar 'f' Modifier.LeftAlt
        do! UIScenario.SendChar 'o' Modifier.None

        do! Scenario.Pause  2000
        }

    let myScenario3 = scenario {
        do! StartMSPaint

        let availableSizes = 
            [
                1.  , Px1
                3.  , Px3
                5.  , Px5
                8.  , Px8
            ]

        do! SelectTool "Line"

        do! SelectSize Px1

        let! bounds = GetDrawingBounds

        let points : (float*Vector*Vector) list ref = ref []
        let generator   = TurtleFractal.Generate 5 100.
        let cx          = 20.
        let cy          = 20.
        let start       = Vector(cx,cy)
        let direction   = Vector(1., 1.)
        ignore <| Turtle.Execute 4. start direction (fun w f t -> points := (w,f,t)::!points) generator

        let gs = !points 
                        |> List.toSeq
                        |> Seq.map (fun (w,f,t) -> 
                            let lz = 
                                match w with
                                | ww when ww <= 2.  -> Px1
                                | ww when ww <= 4.  -> Px3
                                | ww when ww <= 6.5 -> Px5
                                | _                 -> Px8
                            lz,f,t
                            )
                        |> Seq.groupBy (fun (lz,_,_) -> lz)
                        |> Seq.toArray
                        |> Array.rev

        for (lz,ps) in gs do 
            do! SelectSize lz
            for (_,f,t) in ps do
                do! Draw bounds f t
                do! Scenario.Pause  50

        do! Scenario.Pause  2000
        }

    let run = Scenario.RunScenario Map.empty myScenario3

    for result in run.State.Results do
        printfn "Result: %A" result

    0
