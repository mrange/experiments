namespace FolderSize

open SharpDX

type UserInterfaceState =
    {
        DefaultTextFormat   : TextFormatDescriptor
        DefaultForeground   : AnimatedBrush

        CurrentState        : ApplicationState
        PreviousState       : ApplicationState
    }

type UserInterfaceResult<'T> =  
    {
        Value   : 'T
        State   : UserInterfaceState
        Logical : LogicalTree
    }
    static member New v s lt = {Value = v; State = s; Logical = lt}

type UserInterface<'T> = 
    {
        Render  :   UserInterfaceState->LogicalTree->UserInterfaceResult<'T>
    }
    static member New r = {Render = r}

[<AutoOpen>]
module UserInterfaceUtils = 

    let GetMouseState (bounds : RectangleF) (ms : MouseState) = 
        if bounds.Contains ms.Coordinate then
            Inside ms.ButtonState
        else 
            Outside ms.ButtonState

//    let (|Outside|Inside|OutsidePressed|InsidePressed|)  (mbs : MouseButtonStates) (ms : MouseState)  = 
//        let inside  = bounds.Contains(ms.Coordinate)
//        let pressed = ms.ButtonState.Contains(mbs)
//        match inside,pressed with
//        | false , false -> Outside
//        | false , true  -> OutsidePressed
//        | true  , false -> Inside
//        | true  , true  -> InsidePressed


module UserInterface = 
    let Return v : UserInterface<'T> = 
        UserInterface<_>.New <| 
            fun uis lt -> UserInterfaceResult<_>.New v uis LogicalTree.Empty
            
    let Zero<'T> : UserInterface<'T> = 
        UserInterface<_>.New <| 
            fun uis lt -> UserInterfaceResult<'T>.New DefaultOf<'T> uis LogicalTree.Empty

    let ReturnFrom v : UserInterface<'T> = v
    let Yield                                           = Return
    let YieldFrom                                       = ReturnFrom

    let Delay (uig : unit -> UserInterface<'T>) : UserInterface<'T> = 
        UserInterface<_>.New <| 
            fun uis lt -> (uig ()).Render uis lt

    let Run (ui : UserInterface<'T>)            : UserInterface<'T> = 
        UserInterface<_>.New <| fun uis lt -> ui.Render uis lt

    let Bind (l : UserInterface<'U>) (r : 'U->UserInterface<'T>) : UserInterface<'T> = 
        UserInterface<_>.New <| 
            fun uis lt ->

                let llt,rlt =
                    match lt with
                    | Fork (llt, rlt)   -> llt,rlt
                    | _                 -> Empty,Empty

                let lr = l.Render uis llt

                let rr = (r lr.Value).Render lr.State rlt

                UserInterfaceResult<_>.New rr.Value rr.State (LogicalTree.Fork (lr.Logical, rr.Logical))

    let Combine (l : UserInterface<'U>) (r : UserInterface<'T>) : UserInterface<'T> = 
        UserInterface<_>.New <| 
            fun uis lt ->

                let llt,rlt =
                    match lt with
                    | Fork (llt, rlt)   -> llt,rlt
                    | _                 -> Empty,Empty

                let lr = l.Render uis llt

                let rr = r.Render lr.State rlt

                UserInterfaceResult<_>.New rr.Value rr.State (LogicalTree.Fork (lr.Logical, rr.Logical))

    let GetBounds (l : UserInterface<'U>) : UserInterface<RectangleF> = 
        UserInterface<_>.New <| 
            fun uis lt -> 
                let result = Logical.GetBounds uis.CurrentState lt
                UserInterfaceResult<_>.New result uis lt 

    let Leaf
        (bounds         : AnimatedRectangleF                                                    )
        (initialState   : 'TState                                                               )
        (p              : UserInterfaceState->'TState->'T*UserInterfaceState*AnimatedRectangleF*'TState*VisualTree  )
        : UserInterface<'T> =
        UserInterface<_>.New <| 
            fun uis lt -> 
                let state =
                    match lt with
                    | LogicalTree.Leaf (_, ostate, _) ->
                        match ostate with
                        | :? 'TState as s   -> s
                        | _                 -> initialState
                    | _                     -> initialState
                
                let nv,ns,nb,nls,nvt = p uis state
                UserInterfaceResult<_>.New nv ns <| LogicalTree.Leaf (nb, nls, nvt)
                
         
    type LabelState = string
            
    let LabelEx 
        (tfd            : TextFormatDescriptor  ) 
        (foreground     : AnimatedBrush         )
        (bounds         : AnimatedRectangleF    ) 
        (label          : string                ) 
        : UserInterface<unit> =
        let vt = VisualTree.Text (label, tfd, bounds, foreground)
        let lt uis = (),uis,bounds,label,vt
        Leaf bounds "" <| fun uis lts -> lt uis


    let Label 
        (bounds         : AnimatedRectangleF    ) 
        (label          : string                ) 
        : UserInterface<unit> =
        let vt tfd foreground = VisualTree.Text (label, tfd, bounds, foreground)
        let lt uis = (),uis,bounds,label,(vt uis.DefaultTextFormat uis.DefaultForeground)
        Leaf bounds "" <| fun uis lts -> lt uis 

    type ButtonStates = 
        |   Normal
        |   HighLighted
        |   Pressed

    type ButtonState = 
        {
            State   : ButtonStates
            Clicked : int
        }
        static member New s c = {State = s; Clicked = c}
        static member Empty = ButtonState.New Normal 0

    let ButtonEx
        (tfd            : TextFormatDescriptor  ) 
        (stroke         : BrushDescriptor       )
        (mouseOver      : BrushDescriptor       )
        (mousePressed   : BrushDescriptor       )
        (foreground     : BrushDescriptor       )
        (background     : BrushDescriptor       )
        (bounds         : AnimatedRectangleF    ) 
        (label          : string                ) 
        : UserInterface<int> =
        let normal = 
            Normal              ,
            VisualTree.Group
                [
                    VisualTree.Rectangle (Animated.Brush_Solid stroke, Animated.Brush_Solid background, bounds, Animated.Constant 3.F)
                    VisualTree.Text (label, tfd, bounds, Animated.Brush_Solid foreground)
                ]
        let highLighted = 
            HighLighted        ,
            VisualTree.Group
                [
                    VisualTree.Rectangle (Animated.Brush_Solid stroke, Animated.Brush_Solid mouseOver, bounds, Animated.Constant 3.F)
                    VisualTree.Text (label, tfd, bounds, Animated.Brush_Solid foreground)
                ]
        let pressed = 
            Pressed         ,
            VisualTree.Group
                [
                    VisualTree.Rectangle (Animated.Brush_Solid stroke, Animated.Brush_Solid mousePressed, bounds, Animated.Constant 3.F)
                    VisualTree.Text (label, tfd, bounds, Animated.Brush_Solid foreground)
                ]
        Leaf bounds ButtonState.Empty <| 
            fun uis lts -> 
                
                let current     = uis.CurrentState
                let previous    = uis.PreviousState

                let b = bounds uis.CurrentState
                let bts,vt,nlts = 
                    match lts.State, GetMouseState b previous.CurrentMouse, GetMouseState b uis.PreviousState.CurrentMouse with
                    | Pressed   , Inside  pbs   , Inside  cbs   when pbs.Contains(Left) && not (cbs.Contains(Left)) -> highLighted  <++> ButtonState.New HighLighted (lts.Clicked + 1)
                    | Pressed   , Outside pbs   , Inside  cbs   when pbs.Contains(Left) && not (cbs.Contains(Left)) -> highLighted  <++> ButtonState.New HighLighted (lts.Clicked + 1)
                    | Pressed   , _             , Inside  cbs   when cbs.Contains(Left)                             -> pressed      <++> lts 
                    | Pressed   , _             , Outside cbs   when cbs.Contains(Left)                             -> highLighted  <++> lts 
                    | _         , _             , Inside  cbs   when cbs.Contains(Left)                             -> pressed      <++> ButtonState.New Pressed lts.Clicked
                    | _         , _             , Inside  _                                                         -> highLighted  <++> ButtonState.New HighLighted lts.Clicked
                    | Normal    , _             , _                                                                 -> normal       <++> lts
                    | _l        , _             , _                                                                 -> normal       <++> ButtonState.New Normal lts.Clicked

                nlts.Clicked,uis,bounds,nlts,vt
            
        
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

            
    

