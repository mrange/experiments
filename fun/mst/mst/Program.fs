open mst

open System.Windows.Automation

[<EntryPoint>]
let main argv = 

    let myScenario = scenario {
        do! UIScenario.StartWindowedProcess "mspaint.exe" <| ByClass "MSPaintApp"

        do! UIScenario.Invoke <| ByName "Line"

        return 1
        }

    let run = Scenario.RunScenario Map.empty myScenario

    0
