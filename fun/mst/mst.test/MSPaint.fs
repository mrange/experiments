namespace mst.test

open System.Windows

open mst
open mst.lowlevel

module MSPaint = 

    type LineSize =
        |   Px1
        |   Px3
        |   Px5
        |   Px8

    type Color = 
        {   
            Red     : int   
            Green   : int
            Blue    : int
        }
        static member New r g b = {Red = r; Green = g; Blue = b}

    let StartMSPaint = UIScenario.StartWindowedProcess "mspaint.exe"

    let WaitForPopup = Scenario.Retry 5 100

    let SelectTool (toolName : string) = UIScenario.Invoke <| ByName toolName

    let GetDrawingBounds = UIScenario.GetBounds <| ByClass "MSPaintView" 

    let offset = 32.

    let Draw (bounds : Rect) (f : Vector) (t : Vector) = 
        let d = t - f
        ignore <| d.Normalize()
        let o = offset * d + t
        UIScenario.DoMouseGesture   [
                                        LeftClickAndHold<| Point(bounds.Left + f.X, bounds.Top + f.Y)
                                        ReleaseLeft     <| Point(bounds.Left + t.X, bounds.Top + t.Y)
                                        LeftClick       <| Point(bounds.Left + o.X, bounds.Top + o.Y)
                                    ]

    let DrawSomething (toolName : string) (cx : float) (cy : float) (w : float) (h : float) = 
        scenario {
            do! SelectTool toolName

            let! bounds = GetDrawingBounds

            let f = Vector(cx,cy)
            let t = Vector(cx + w,cy + h)

            do! Draw bounds f t
        }

    let SelectSize (lz : LineSize) : Scenario<unit> = 
        scenario {
            let toolName = 
                match lz with
                |   Px1 -> "1px"
                |   Px3 -> "3px"
                |   Px5 -> "5px"
                |   Px8 -> "8px"

            do! UIScenario.Invoke <| ByName "Size"
            do! UIScenario.Invoke <| ByName toolName

            return ()
        }

    let SetColor (c : Color)  : Scenario<unit> = 
        let colorText (i : int) = 
            match i with
            | i when i >= 0 && i <= 255 -> i.ToString (System.Globalization.CultureInfo.InvariantCulture)
            | _                         -> "0"
        scenario {
            do! UIScenario.SetCurrentElement <| ByClass "#32770"

            do! UIScenario.SetText (colorText c.Blue)   <| ById "708"
            do! UIScenario.SetText (colorText c.Green)  <| ById "707"
            do! UIScenario.SetText (colorText c.Red)    <| ById "706"

            do! UIScenario.Invoke <| ById "1"
        }

    let SelectColor (c : Color)  : Scenario<unit> = 
        scenario {
            do! UIScenario.Invoke <| ByName "Edit colors"
            do! WaitForPopup <| SetColor c
        }

    let Fill (c : Color) (cx : float) (cy : float) = 
        scenario {
            do! SelectColor c
            do! SelectTool "Fill with color"

            let! bounds = GetDrawingBounds

            do! UIScenario.DoMouseGesture   [
                                                LeftClick       <| Point(bounds.Left + cx, bounds.Top + cy)
                                            ]
        }


    let DrawLine        = DrawSomething "Line"
    let DrawOval        = DrawSomething "Oval"
    let DrawRectangle   = DrawSomething "Rectangle"

    let ConfirmSaveAs : Scenario<unit> = 
        scenario {
            do! WaitForPopup (UIScenario.FocusElement <| ByName "Confirm Save As")
            do! UIScenario.SendChar 'y' Modifier.LeftAlt
        }

    let SaveFile fileName : Scenario<unit> = 
        scenario {
            do! UIScenario.SendChar 'f' Modifier.LeftAlt
            do! UIScenario.SendChar 'a' Modifier.None

            do! WaitForPopup (UIScenario.SetCurrentElement <| ByName "Save As")

            do! UIScenario.SetText fileName <| ById "1001"

            do! UIScenario.SendChar 's' Modifier.LeftAlt

            let! _ = Scenario.Optional ConfirmSaveAs

            return ()
        }

