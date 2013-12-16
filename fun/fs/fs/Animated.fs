namespace FolderSize

open SharpDX
open SharpDX

type AnimationEase      = Time->Time->float32->float32->ApplicationState->float32

type AnimatedFloat      = ApplicationState->float32
type AnimatedVector2    = ApplicationState->Vector2
type AnimatedRectangleF = ApplicationState->RectangleF
type AnimatedBrush      = ApplicationState->BrushDescriptor*float32
type AnimatedMatrix     = ApplicationState->Matrix3x2


module Animated = 

    let Constant (v : 'T) : ApplicationState->'T = fun s -> v

    let Ease_Linear (b : Time) (e : Time) (f : float32) (t : float32) (state : ApplicationState) = 
        if state.CurrentTime < b then f
        elif state.CurrentTime > e then t
        else    
            let m = (state.CurrentTime - b) / (e - b)
            m*(state.CurrentTime - f) + f   

    let Float (ease : AnimationEase) b e f t : AnimatedFloat = 
        ease b e f t
                    
    let Vector2 (ease : AnimationEase) b e (f : Vector2) (t : Vector2) : AnimatedVector2 =
        let x = ease b e f.X t.X 
        let y = ease b e f.Y t.Y 
        fun time -> Vector2 (x time, y time)

    let RectangleF (ease : AnimationEase) b e (f : RectangleF) (t : RectangleF) : AnimatedRectangleF =
        let x = ease b e f.X t.X 
        let y = ease b e f.Y t.Y 
        let w = ease b e f.Width t.Width 
        let h = ease b e f.Width t.Width 
        fun time -> RectangleF (x time,y time,w time,h time)

    let Brush_Opacity (ease : AnimationEase) (v : BrushDescriptor) b e (f : float32) (t : float32) : AnimatedBrush = 
        let o = ease b e f t 
        fun time -> v, o time

    let Brush_Solid (v : BrushDescriptor) : AnimatedBrush = 
        fun time -> v, 1.F

    let Matrix_Rotation (ease : AnimationEase) b e (f : float32) (t : float32) : AnimatedMatrix = 
        let d = ease b e f t 
        fun time -> Matrix3x2.Rotation <| d time

    let Matrix_Translation (ease : AnimationEase) b e (f : Vector2) (t : Vector2) : AnimatedMatrix = 
        let d = Vector2 ease b e f t 
        fun time -> Matrix3x2.Translation <| d time

    let Matrix_Scale (ease : AnimationEase) b e (f : Vector2) (t : Vector2) : AnimatedMatrix = 
        let d = Vector2 ease b e f t 
        fun time -> Matrix3x2.Scaling(d time)

[<AutoOpen>]
module AnimatedUtils = 
    let ( <**> ) (l : AnimatedMatrix) (r : AnimatedMatrix) : AnimatedMatrix = 
        fun time -> let left    = l time
                    let right   = r time
                    Matrix3x2.Multiply (left, right)

