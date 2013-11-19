open mst

open System.Windows
open System.Windows.Media
open System.Windows.Automation

open mst.lowlevel

module LSystem =
    type Alphabet =
        | DoNothing     // X
        | Forward       // F
        | TurnLeft      // -
        | TurnRight     // +
        | Store         // [
        | Restore       // ]

    let Angle = 25.

    let Initial = [DoNothing]

    let Rule (a : Alphabet) = 
        match a with
                     // F       -        [     [     X         ]       +         X         ]       +         F       [     +         F       X         ]       -        X 
        | DoNothing -> [Forward;TurnLeft;Store;Store;DoNothing;Restore;TurnRight;DoNothing;Restore;TurnRight;Forward;Store;TurnRight;Forward;DoNothing;Restore;TurnLeft;DoNothing]
                     // F       F
        | Forward   -> [Forward;Forward]
        | _         -> [a]

    let rec Generate =  function 
                        | 0 ->  Initial         
                        | n ->  let previous = Generate (n - 1)
                                previous |> List.collect Rule

    let RotateLeft  =   let m = Matrix.Identity
                        m.Rotate(Angle)
                        m

    let RotateRight =   let m = Matrix.Identity
                        m.Rotate(-Angle)
                        m



[<EntryPoint>]
let main argv = 

    let StartMSPaint = UIScenario.StartWindowedProcess "mspaint.exe"
    let StartNotepad = UIScenario.StartWindowedProcess "notepad.exe"

    let WaitForPopup = Scenario.Retry 5 100

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
            do! UIScenario.FocusElement <| ById "1001"

            do! UIScenario.SendText fileName

            do! UIScenario.SendChar 's' Modifier.LeftAlt

            let! x = Scenario.Optional ConfirmSaveAs

            return ()
        }

    let myScenario = scenario {
        do! StartMSPaint

        do! DrawOval        100. 100. 200. 200.
        do! DrawRectangle   200. 200. 100. 100.

        do! SaveFile "tester.png"

        do! Scenario.Pause  2000

        return 1
        }

    let myScenario2 = scenario {
        do! StartNotepad

        do! Scenario.Pause  500

        do! UIScenario.SendText "Good game!"

        do! UIScenario.SendChar 'f' Modifier.LeftAlt
        do! UIScenario.SendChar 'o' Modifier.None

        do! Scenario.Pause  2000

        return 1
        }

    let run = Scenario.RunScenario Map.empty myScenario

    for result in run.State.Results do
        printfn "Result: %A" result

    0
