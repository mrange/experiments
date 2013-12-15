namespace FolderSize

open SharpDX

type Time = float32

type MouseButtonStates = 
    | Left
    | Middle
    | Right

type MouseStates = 
    | Outside   of Set<MouseButtonStates>
    | Inside    of Set<MouseButtonStates>

type MouseState = 
    {
        ButtonState : Set<MouseButtonStates>
        Coordinate  : Vector2
    }
    static member New bs c = {ButtonState = bs; Coordinate = c}

type ApplicationState = 
    {
        CurrentTime     : Time
        CurrentMouse    : MouseState
    }
    static member New ct cm = {CurrentTime = ct; CurrentMouse = cm}

