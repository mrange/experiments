open mst

open System.Windows.Automation

[<EntryPoint>]
let main argv = 

    let myScenario = scenario {
        do! UIScenario.StartWindowedProcess "mspaint.exe" "MSPaintApp"

        do! UIScenario.SelectElement "Input"

        do! UIScenario.Invoke "OK"

        let! text = UIScenario.GetText "Input"

        return 1
        }

    let run = Scenario.RunScenario Map.empty myScenario

    0
