namespace FolderSize

open SharpDX

type MouseButton = 
    | Left
    | Middle
    | Right

type UserInterfaceState =
    {
        DefaultTextFormat   : TextFormatDescriptor
        DefaultForeground   : BrushDescriptor

        CurrentTime         : float32
        MouseMovement       : Vector2
        MouseCoordinate     : Vector2
        MouseState          : Set<MouseButton>
    }

type UserInterfaceResult<'T> =  
    {
        Value   : 'T
        State   : UserInterfaceState
        Logical : LogicalTree
    }
    static member New v s lt = {Value = v; State = s; Logical = lt}

type UserInterface<'T> = UserInterfaceState->LogicalTree->UserInterfaceResult<'T>

module UserInterface = 
    let Return v : UserInterface<'T> = 
        fun s lt -> UserInterfaceResult<_>.New v s LogicalTree.Empty
            
    let Zero : UserInterface<'T> = 
        fun s lt -> UserInterfaceResult<_>.New DefaultOf<'T> s LogicalTree.Empty

    let ReturnFrom v : UserInterface<'T> = v
    let Yield                                           = Return
    let YieldFrom                                       = ReturnFrom

    let Delay (uig : unit -> UserInterface<'T>) : UserInterface<'T> = 
        fun s lt -> uig () s lt

    let Run (ui : UserInterface<'T>)            : UserInterface<'T> = 
        fun s lt -> ui s lt

    let Bind (l : UserInterface<'U>) (r : 'U->UserInterface<'T>) : UserInterface<'T> = 
        fun s lt ->

            let llt,rlt =
                match lt with
                | Fork (llt, rlt)   -> llt,rlt
                | _                 -> Empty,Empty

            let lr = l s llt

            let rr = (r lr.Value) lr.State rlt

            UserInterfaceResult<_>.New rr.Value rr.State (LogicalTree.Fork (lr.Logical, rr.Logical))

    let Combine (l : UserInterface<'U>) (r : UserInterface<'T>) : UserInterface<'T> = 
        fun s lt ->

            let llt,rlt =
                match lt with
                | Fork (llt, rlt)   -> llt,rlt
                | _                 -> Empty,Empty

            let lr = l s llt

            let rr = r lr.State rlt

            UserInterfaceResult<_>.New rr.Value rr.State (LogicalTree.Fork (lr.Logical, rr.Logical))

    let GetBounds (l : UserInterface<'U>) : UserInterface<RectangleF> = 
        fun s lt -> 
            let result = Logical.GetBounds s.CurrentTime lt
            UserInterfaceResult<_>.New result s lt 

    type LabelState = string
            
    let LabelEx 
        (tfd            : TextFormatDescriptor  ) 
        (foreground     : AnimatedBrush         )
        (bounds         : AnimatedRectangleF    ) 
        (label          : string                ) 
        : UserInterface<unit> =
        fun s lt -> 
            let state       = label
            UserInterfaceResult<_>.New () s <| LogicalTree.Leaf (bounds, state, VisualTree.Text (state, tfd, bounds, foreground))

    let Label 
        (bounds         : AnimatedRectangleF    ) 
        (label          : string                ) 
        : UserInterface<unit> =
        fun s lt -> 
            let state       = label
            UserInterfaceResult<_>.New () s <| LogicalTree.Leaf (bounds, state, VisualTree.Text (state, s.DefaultTextFormat, bounds, Animated.Brush_Solid s.DefaultForeground))

    type ButtonState = int
        
    let ButtonEx
        (tfd            : TextFormatDescriptor  ) 
        (stroke         : BrushDescriptor       )
        (mouseOver      : BrushDescriptor       )
        (foreground     : BrushDescriptor       )
        (background     : BrushDescriptor       )
        (bounds         : AnimatedRectangleF    ) 
        (label          : string                ) 
        : UserInterface<int> =
        fun s lt -> 
            let state       = 0
            let vt = 
                VisualTree.Group
                    [
                        VisualTree.Rectangle (Animated.Brush_Solid stroke, Animated.Brush_Solid background, bounds, Animated.Constant 3.F)
                        VisualTree.Text (label, tfd, bounds, Animated.Brush_Solid foreground)
                    ]
            UserInterfaceResult<_>.New 0 s <| LogicalTree.Leaf (bounds, state, vt)

            
        
[<AutoOpen>]
module UserInterfaceBuilder =

    type UserInterfaceBuilder() =
        member x.Return(value)                  = UserInterface.Return value
        member x.Zero()                         = UserInterface.Zero
        member x.ReturnFrom(value)              = UserInterface.ReturnFrom value
        member x.Yield(value)                   = UserInterface.Yield value
        member x.YieldFrom(value)               = UserInterface.YieldFrom value
        member x.Delay(func)                    = UserInterface.Delay func
        member x.Run(func)                      = UserInterface.Run func
        member x.Bind(func, comp)               = UserInterface.Bind func comp
        member x.Combine(expr1, expr2)          = UserInterface.Combine expr1 expr2
//        member x.For(expr1, expr2)              = UserInterface.For expr1 expr2
//        member x.While(expr1, expr2)            = UserInterface.While expr1 expr2

    let inline ( >>= ) l r = UserInterface.Bind l r
    let inline ( >>+ ) l r = UserInterface.Combine l r
    
    let ui = UserInterfaceBuilder()

            
    

