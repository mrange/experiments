open mst

open System.Windows
open System.Windows.Automation

open mst.lowlevel

[<EntryPoint>]
let main argv = 

    let StartMSPaint = UIScenario.StartWindowedProcess "mspaint.exe"

    let DrawSomething (toolName : string) (cx : float) (cy : float) (w : float) (h : float) = 
        scenario {
            do! UIScenario.Invoke <| ByName toolName

            let! bounds = UIScenario.GetBounds <| ByClass "MSPaintView"

            do! UIScenario.DoMouseGesture   [
                                                LeftClickAndHold<| Point(cx + bounds.Left, cy + bounds.Top)
                                                ReleaseLeft     <| Point(cx + bounds.Left + w, cy + bounds.Top + h)
                                            ]
            return ()
        }

    let DrawOval        = DrawSomething "Oval"
    let DrawRectangle   = DrawSomething "Rectangle"

    let myScenario = scenario {
        do! StartMSPaint

        do! DrawOval        100. 100. 200. 200.
        do! DrawRectangle   200. 200. 100. 100.

        do! UIScenario.SendText "GGG"
        do! UIScenario.SendChar 'o' Modifier.LeftControl

        do! Scenario.Pause  2000

        return 1
        }

    let run = Scenario.RunScenario Map.empty myScenario

    0
