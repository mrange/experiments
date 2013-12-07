namespace FolderSize

open SharpDX
open SharpDX

type VisualTree =
    |   Empty           
    |   Rectangle       of Stroke:AnimatedBrush*Fill:AnimatedBrush*Rect:AnimatedRectangleF*StrokeWidth:AnimatedFloat
    |   Line            of Point0:AnimatedVector2*Point1:AnimatedVector2*Brush:AnimatedBrush*StrokeWidth:AnimatedFloat
    |   Text            of Text:string*TextFormat:TextFormatDescriptor*LayoutRect:AnimatedRectangleF*Foreground:AnimatedBrush
    |   Transform       of Transform:AnimatedMatrix*Child:VisualTree
    |   Group           of Children:VisualTree list
    |   Fork            of Left:VisualTree*Right:VisualTree
    |   State           of State:obj*Child:VisualTree

module Visual = 

    let rec HasVisuals (vt : VisualTree) =
        match vt with
        | Empty             -> false
        | Rectangle _       -> true
        | Line _            -> true
        | Text _            -> true
        | Transform (t,c)   -> HasVisuals c
        | Group cs          -> cs |> ListEx.any (fun c -> HasVisuals c)
        | Fork (l,r)        -> (HasVisuals l) && (HasVisuals r)
        | State (_,c)       -> HasVisuals c

    let rec RenderTreeImpl 
        (time       : float32                                       ) 
        (rt         : Direct2D1.RenderTarget                        ) 
        (tfc        : TextFormatDescriptor->DirectWrite.TextFormat  ) 
        (bc         : BrushDescriptor*float32->Direct2D1.Brush      ) 
        (transform  : Matrix3x2                                     ) 
        (pixelScale : float32                                       )  
        (vt         : VisualTree                                    ) 
        = 
        match vt with 
        |   Empty   -> ()
        |   Rectangle (s,f,r,sw) ->
                let rect        = r time
                let strokeWidth = pixelScale * sw time
                let fill        = f time
                let stroke      = s time

                let bfill       = bc fill
                let bstroke     = bc stroke

                if bfill <> null then rt.FillRectangle (rect, bfill)
                if bstroke <> null && strokeWidth > 0.F then rt.DrawRectangle (rect, bstroke, strokeWidth)
        |   Line (p0,p1,b,sw) ->
                let point0      = p0 time
                let point1      = p1 time
                let stroke      = b time
                let strokeWidth = pixelScale * sw time

                let bstroke     = bc stroke

                if bstroke <> null && strokeWidth >= 0.F then rt.DrawLine (point0, point1, bstroke, strokeWidth)
        |   Text (t,tf,lr,fg) ->
                let layoutRect  = lr time
                let foreground  = fg time
                let atf         = TextFormatDescriptor.New tf.FontFamily (pixelScale * tf.FontSize)
                let textFormat  = tfc atf

                let bforeground = bc foreground

                if bforeground <> null then rt.DrawText(t, textFormat, layoutRect, bforeground)
        |   Transform (t,c) ->
                let newTransform    = t time

                let fullTransform   = transform <*> newTransform

                // Compute pixel scale (approximation)
                let getLocalLength x y  = (Matrix3x2.TransformPoint (fullTransform, Vector2(x,y))).Length()

                let iscale          = getLocalLength 1.F 1.F

                rt.Transform <- fullTransform
                                
                RenderTreeImpl time rt tfc bc fullTransform (1.F / iscale) c

                rt.Transform <- transform
        |   Group (cs) ->
                for branch in cs do
                    RenderTreeImpl time rt tfc bc transform pixelScale branch 
        |   Fork (l,r) ->
                RenderTreeImpl time rt tfc bc transform pixelScale l 
                RenderTreeImpl time rt tfc bc transform pixelScale r
        |   State (_,c) ->
                RenderTreeImpl time rt tfc bc transform pixelScale c

    let RenderTree 
        (time       : float32                                       ) 
        (rt         : Direct2D1.RenderTarget                        ) 
        (tfc        : TextFormatDescriptor->DirectWrite.TextFormat  ) 
        (bc         : BrushDescriptor*float32->Direct2D1.Brush      ) 
        (vt         : VisualTree                                    ) 
        = 
        RenderTreeImpl time rt tfc bc Matrix3x2.Identity 1.0F vt
