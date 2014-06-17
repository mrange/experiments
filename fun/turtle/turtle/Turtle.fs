// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
// If you cannot locate the  Microsoft Public License, please send an email to
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

namespace TurtlePower

open SharpDX

module Turtle = 

    type Color      = | Brown
                      | LimeGreen
                      | Lime
                      | MediumVioletRed

    type DrawLine   = Color->float32->Vector2->Vector2->unit

    type State =
        {
            Color       : Color
            Width       : float32
            Position    : Vector2
            Direction   : Vector2
            DrawLine    : DrawLine
        }
        static member New c w p d dl = 
            {Color = c; Width = w; Position = p; Direction = Normalize d; DrawLine = dl;}
        static member UnsafeNew c w p d dl = 
            {Color = c; Width = w; Position = p; Direction = d; DrawLine = dl;}

    type Result<'T> =
        {
            Value       : 'T
            State       : State
        }
        static member New v s = {Value = v; State = s;}

    type Movement<'T> = State -> Result<'T>

    let Return v                        : Movement<'T>      = fun s ->   Result<_>.New v s
    let Zero ()                         : Movement<unit>    = fun s ->   Result<_>.New () s
    let ReturnFrom (t : Movement<'T>)   : Movement<'T>      = t
    let Yield                                               = Return
    let YieldFrom                                           = ReturnFrom
    let Delay (tg : unit -> Movement<'T>) : Movement<'T>    = fun s -> tg () s
    let Run (t : Movement<'T>)            : Movement<'T>    = fun s -> t s

    let Bind (l : Movement<'T>) (r : 'T -> Movement<'U>)    : Movement<'U> = 
        fun s ->    
            let m = l s
            (r m.Value) m.State
        

    let Combine (l : Movement<unit>) (r : Movement<'U>)     : Movement<'U> =
        fun s ->   
            let m = l s
            r m.State

    let For (seq : seq<'T>) (r : 'T -> Movement<unit>)      : Movement<unit> =
        fun s ->   
            let mutable state   = s
            for v in seq do
                let mm = r v state 
                state <- mm.State
            Result<_>.New () state

    let While (e : unit -> bool) (r : Movement<unit>)       : Movement<unit> =
        fun s ->   
            let mutable state   = s
            while e () do
                let mm = r state
                state <- mm.State
            Result<_>.New () state
        
    let RunAndReturn (t : Movement<'T>)                     : Movement<'T>    = 
        fun s ->    
            let m = t s
            Result<_>.New m.Value s

    let Color (c : Color)                                   : Movement<unit> = 
        fun s -> 
            let ss = State.UnsafeNew c s.Width s.Position s.Direction s.DrawLine
            Result<_>.New () ss
        

    let Width (w : float32)                                 : Movement<unit> = 
        fun s -> 
            let ss = State.UnsafeNew s.Color w s.Position s.Direction s.DrawLine
            Result<_>.New () ss
        

    let Turn (a : float32)                                  : Movement<unit> = 
        fun s -> 
            let r = Matrix3x2.Rotation (Deg2Rad * a)
            let d = Matrix3x2.TransformPoint (r, s.Direction)
            let ss = State.UnsafeNew s.Color s.Width s.Position d s.DrawLine
            Result<_>.New () ss
        

    let Forward (v : float32)                               : Movement<unit> = 
        fun s -> 
            let p = s.Position + v*s.Direction 
            let ss = State.UnsafeNew s.Color s.Width p s.Direction s.DrawLine
            ss.DrawLine s.Color s.Width s.Position p
            Result<_>.New () ss
        

    let Execute (c : Color) (w : float32) (p : Vector2) (d : Vector2) (dl : DrawLine) (t : Movement<'T>) =
        let s = State.New c w p d dl
        t s

[<AutoOpen>]
module TurtleBuilder =

    type TurtleBuilder () =
        member x.Return (value)         = Turtle.Return value
        member x.Zero ()                = Turtle.Zero
        member x.ReturnFrom (value)     = Turtle.ReturnFrom value
        member x.Yield (value)          = Turtle.Yield value
        member x.YieldFrom (value)      = Turtle.YieldFrom value
        member x.Delay (func)           = Turtle.Delay func
        member x.Run (func)             = Turtle.Run func
        member x.Bind (func, comp)      = Turtle.Bind func comp
        member x.Combine (expr1, expr2) = Turtle.Combine expr1 expr2
        member x.For (expr1, expr2)     = Turtle.For expr1 expr2
        member x.While (expr1, expr2)   = Turtle.While expr1 expr2

    let inline ( >>= ) l r = Turtle.Bind l r
    let inline ( >>+ ) l r = Turtle.Combine l r
    
    let turtle = TurtleBuilder ()
