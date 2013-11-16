open mst

open System.Windows.Automation

[<EntryPoint>]
let main argv = 

    let DrawEllipse (cx : float) (cy : float) (w : float) (h : float)= 
        scenario {
            do! UIScenario.Invoke <| ByName "Oval"

            let x : AutomationElement = null

            

            return ()
        }

    let myScenario = scenario {
        do! UIScenario.StartWindowedProcess "mspaint.exe" <| ByClass "MSPaintApp"

        do! UIScenario.Invoke <| ByName "Line"

        return 1
        }

    let run = Scenario.RunScenario Map.empty myScenario

    0
