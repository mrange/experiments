namespace silberman

open SharpDX

open Fundamental
open Device

module public Animated = 

    let Constant (v : 'T) : ApplicationState->'T = fun s -> v

    module Ease = 
        let Linear (b : Time) (e : Time) (f : float32) (t : float32) (state : ApplicationState) = 
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

    module Brush =
        let Opacity (ease : AnimationEase) (v : BrushDescriptor) b e (f : float32) (t : float32) : AnimatedBrush = 
            let o = ease b e f t 
            fun time -> v, o time

        let Solid (v : BrushDescriptor) : AnimatedBrush = 
            fun time -> v, 1.F

    module Matrix =
        let Rotation (ease : AnimationEase) b e (f : float32) (t : float32) : AnimatedMatrix = 
            let d = ease b e f t 
            fun time -> Matrix3x2.Rotation <| d time

        let Translation (ease : AnimationEase) b e (f : Vector2) (t : Vector2) : AnimatedMatrix = 
            let d = Vector2 ease b e f t 
            fun time -> Matrix3x2.Translation <| d time

        let Scale (ease : AnimationEase) b e (f : Vector2) (t : Vector2) : AnimatedMatrix = 
            let d = Vector2 ease b e f t 
            fun time -> Matrix3x2.Scaling(d time)

