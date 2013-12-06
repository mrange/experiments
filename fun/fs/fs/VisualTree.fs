namespace FolderSize

open SharpDX
open SharpDX.Direct2D1

[<AutoOpen>]
module Visual = 

    type AnimatedFloat      = float32->float32
    type AnimatedVector2    = float32->Vector2
    type AnimatedRectangleF = float32->RectangleF
    type AnimatedBrush      = float32->BrushDescriptor*float32
    type AnimatedMatrix     = float32->Matrix3x2

    type VisualTree =
        |   Empty           
        |   Rectangle       of Stroke:AnimatedBrush*Fill:AnimatedBrush*Rect:AnimatedRectangleF*StrokeWidth:AnimatedFloat
        |   Line            of Point0:AnimatedVector2*Point1:AnimatedVector2*Brush:AnimatedBrush
        |   Text            of Text:string*TextFormat:DirectWrite.TextFormat*LayoutRect:AnimatedRectangleF*Foreground:AnimatedBrush
        |   Transform       of Transform:AnimatedMatrix*Child:VisualTree
        |   Group           of Children:VisualTree list
        |   Fork            of Left:VisualTree*Right:VisualTree
        |   State           of State:obj*Child:VisualTree

    let rec RenderTree (time : float32) (rt : RenderTarget) (bc : BrushDescriptor*float32->Brush)  (vt : VisualTree) = 
        match vt with 
        |   Empty   -> ()
        |   Rectangle (Stroke = s; Fill = f; Rect = r; StrokeWidth = sw) ->
                let rect        = r time
                let strokeWidth = sw time
                let fill        = f time
                let stroke      = s time

                let bfill       = bc fill
                let bstroke     = bc stroke

                if bfill <> null then rt.FillRectangle (rect, bfill)
                if bstroke <> null then rt.DrawRectangle (rect, bstroke, strokeWidth, null)
        |   Line (Point0 = p0; Point1 = p1; Brush = b) ->
                let point0  = p0 time
                let point1  = p1 time
                let stroke  = b time

                let bstroke     = bc stroke

                if bstroke <> null then rt.DrawLine (point0, point1, bstroke)
        |   Text (Foreground = fg; Text = t; TextFormat = tf; LayoutRect = lr) ->
                let layoutRect  = lr time
                let foreground  = fg time

                let bforeground = bc foreground

                if bforeground <> null then rt.DrawText(t, tf, layoutRect, bforeground)
        |   Transform (Transform = t; Child = c) ->
                let transform = t time

                let old = rt.Transform

                rt.Transform <- old <*> transform
                                
                RenderTree time rt bc c

                rt.Transform <- old
        |   Group (Children = c) ->
                for branch in c do
                    RenderTree time rt bc branch 
        |   Fork (Left = l; Right = r) ->
                RenderTree time rt bc l 
                RenderTree time rt bc r
        |   State (Child = c) ->
                RenderTree time rt bc c

    let ( <**> ) (l : AnimatedMatrix) (r : AnimatedMatrix) : AnimatedMatrix = 
        fun time -> let left    = l time
                    let right   = r time
                    Matrix3x2.Multiply (left, right)

module Animated = 

    type Ease = float32->float32->float32->float32->float32->float32

    let Constant (v : 'T) : float32->'T = 
        fun time -> v

    let Ease_Linear (f : float32) (t : float32) (b : float32) (e : float32) (time : float32) = 
        if time < b then f
        elif time > e then t
        else    
            let m = (time - b) / (e - b)
            m*(time - f) + f   

    let Float (ease : Ease) f t b e : AnimatedFloat = 
        ease f t b e
                    
    let Vector2 (ease : Ease) (f : Vector2) (t : Vector2) b e : AnimatedVector2 =
        let x = ease f.X t.X b e
        let y = ease f.Y t.Y b e
        fun time -> Vector2 (x time, y time)

    let RectangleF (ease : Ease) (f : RectangleF) (t : RectangleF) b e : AnimatedRectangleF =
        let x = ease f.X t.X b e
        let y = ease f.Y t.Y b e
        let w = ease f.Width t.Width b e
        let h = ease f.Width t.Width b e
        fun time -> RectangleF (x time,y time,w time,h time)

    let Brush_Opacity (ease : Ease) (v : BrushDescriptor) (f : float32) (t : float32) b e : AnimatedBrush = 
        let o = ease f t b e
        fun time -> v, o time

    let Brush_Solid (v : BrushDescriptor) : AnimatedBrush = 
        fun time -> v, 1.F

    let Matrix_Rotation (ease : Ease) (f : float32) (t : float32) b e : AnimatedMatrix = 
        let d = ease f t b e
        fun time -> Matrix3x2.Rotation <| d time

    let Matrix_Translation (ease : Ease) (f : Vector2) (t : Vector2) b e : AnimatedMatrix = 
        let d = Vector2 ease f t b e
        fun time -> Matrix3x2.Translation <| d time

    let Matrix_Scale (ease : Ease) (f : Vector2) (t : Vector2) b e : AnimatedMatrix = 
        let d = Vector2 ease f t b e
        fun time -> Matrix3x2.Scaling(d time)

