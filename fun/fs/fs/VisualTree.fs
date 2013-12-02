namespace FolderSize

open SharpDX
open SharpDX.Direct2D1

[<AutoOpen>]
module Visual = 

    type AnimatedFloat      = float32->float32
    type AnimatedVector2    = float32->Vector2
    type AnimatedRectangleF = float32->RectangleF
    type AnimatedBrush      = float32->Brush
    type AnimatedMatrix     = float32->Matrix3x2

    type VisualTree =
        |   Rectangle   of Stroke:AnimatedBrush*Fill:AnimatedBrush*Rect:AnimatedRectangleF*StrokeWidth:AnimatedFloat*StrokeStyle:StrokeStyle
        |   Line        of Point0:AnimatedVector2*Point1:AnimatedVector2*Brush:AnimatedBrush
        |   Text        of Text:string*TextFormat:DirectWrite.TextFormat*LayoutRect:AnimatedRectangleF*Foreground:AnimatedBrush
        |   Node        of Transform:AnimatedMatrix option*Opacity:AnimatedFloat option*Nodes:VisualTree list

    let rec RenderTree (time : float32) (rt : RenderTarget) (vt : VisualTree) = 
        match vt with 
        |   Rectangle (Stroke = s; Fill = f; Rect = r; StrokeWidth = sw; StrokeStyle = ss) ->
                let rect        = r time
                let strokeWidth = sw time
                let fill        = f time
                let stroke      = s time
                if fill <> null then rt.FillRectangle (rect, fill)
                if stroke <> null then rt.DrawRectangle (rect, stroke, strokeWidth, ss)
        |   Line (Point0 = p0; Point1 = p1; Brush = b) ->
                let point0  = p0 time
                let point1  = p1 time
                let brush   = b time
                if brush <> null then rt.DrawLine (point0, point1, brush)
        |   Text (Foreground = fg; Text = t; TextFormat = tf; LayoutRect = lr) ->
                let layoutRect  = lr time
                let foreground  = fg time
                if foreground <> null then rt.DrawText(t, tf, layoutRect, foreground)
        |   Node (Transform = t; Opacity = o; Nodes = n) ->
                let transform = 
                    match t with
                    |   Some tt ->  let transform = tt time
                                    let currentTransform = rt.Transform
                                    rt.Transform <- Matrix3x2.Multiply(currentTransform, transform)
                                    Some currentTransform
                    |   _       ->  None

                for node in n do
                    RenderTree time rt node 

                match transform with
                | Some tt   -> rt.Transform <- tt
                | _         -> ()

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

    let Brush (ease : Ease) (v : Brush) (f : float32) (t : float32) b e : AnimatedBrush = 
        let o = ease f t b e
        fun time -> v.Opacity <- o time
                    v

    let Matrix_Rotation (ease : Ease) (f : float32) (t : float32) b e : AnimatedMatrix = 
        let d = ease f t b e
        fun time -> Matrix3x2.Rotation <| d time

    let Matrix_Translation (ease : Ease) (f : Vector2) (t : Vector2) b e : AnimatedMatrix = 
        let d = Vector2 ease f t b e
        fun time -> Matrix3x2.Translation <| d time

    let Matrix_Scale (ease : Ease) (f : Vector2) (t : Vector2) b e : AnimatedMatrix = 
        let d = Vector2 ease f t b e
        fun time -> Matrix3x2.Scaling(d time)

