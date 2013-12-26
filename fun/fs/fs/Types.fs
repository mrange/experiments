namespace FolderSize

open SharpDX

type Time = float32

[<StructuralEquality>]
[<StructuralComparison>]
type MouseButtonStates = 
    | Left
    | Middle
    | Right

[<NoEquality>]
[<NoComparison>]
type MouseStates = 
    | Outside   of Set<MouseButtonStates>
    | Inside    of Set<MouseButtonStates>

[<NoEquality>]
[<NoComparison>]
type MouseState = 
    {
        ButtonState : Set<MouseButtonStates>
        Coordinate  : Vector2
    }
    static member New bs c = {ButtonState = bs; Coordinate = c}

[<NoEquality>]
[<NoComparison>]
type ApplicationState = 
    {
        CurrentTime     : Time
        CurrentMouse    : MouseState
    }
    static member New ct cm = {CurrentTime = ct; CurrentMouse = cm}

