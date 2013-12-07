namespace FolderSize

open SharpDX
open SharpDX.Direct2D1

type AnimationEase      = float32->float32->float32->float32->float32->float32

type AnimatedFloat      = float32->float32
type AnimatedVector2    = float32->Vector2
type AnimatedRectangleF = float32->RectangleF
type AnimatedBrush      = float32->BrushDescriptor*float32
type AnimatedMatrix     = float32->Matrix3x2


module Animated = 

    let Constant (v : 'T) : float32->'T = 
        fun time -> v

    let Ease_Linear (f : float32) (t : float32) (b : float32) (e : float32) (time : float32) = 
        if time < b then f
        elif time > e then t
        else    
            let m = (time - b) / (e - b)
            m*(time - f) + f   

    let Float (ease : AnimationEase) f t b e : AnimatedFloat = 
        ease f t b e
                    
    let Vector2 (ease : AnimationEase) (f : Vector2) (t : Vector2) b e : AnimatedVector2 =
        let x = ease f.X t.X b e
        let y = ease f.Y t.Y b e
        fun time -> Vector2 (x time, y time)

    let RectangleF (ease : AnimationEase) (f : RectangleF) (t : RectangleF) b e : AnimatedRectangleF =
        let x = ease f.X t.X b e
        let y = ease f.Y t.Y b e
        let w = ease f.Width t.Width b e
        let h = ease f.Width t.Width b e
        fun time -> RectangleF (x time,y time,w time,h time)

    let Brush_Opacity (ease : AnimationEase) (v : BrushDescriptor) (f : float32) (t : float32) b e : AnimatedBrush = 
        let o = ease f t b e
        fun time -> v, o time

    let Brush_Solid (v : BrushDescriptor) : AnimatedBrush = 
        fun time -> v, 1.F

    let Matrix_Rotation (ease : AnimationEase) (f : float32) (t : float32) b e : AnimatedMatrix = 
        let d = ease f t b e
        fun time -> Matrix3x2.Rotation <| d time

    let Matrix_Translation (ease : AnimationEase) (f : Vector2) (t : Vector2) b e : AnimatedMatrix = 
        let d = Vector2 ease f t b e
        fun time -> Matrix3x2.Translation <| d time

    let Matrix_Scale (ease : AnimationEase) (f : Vector2) (t : Vector2) b e : AnimatedMatrix = 
        let d = Vector2 ease f t b e
        fun time -> Matrix3x2.Scaling(d time)

[<AutoOpen>]
module AnimatedUtils = 
    let ( <**> ) (l : AnimatedMatrix) (r : AnimatedMatrix) : AnimatedMatrix = 
        fun time -> let left    = l time
                    let right   = r time
                    Matrix3x2.Multiply (left, right)

