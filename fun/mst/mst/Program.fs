open mst

open System.Windows.Automation

[<EntryPoint>]
let main argv = 

    let ss = ScenarioState.New Map.empty Map.empty [] []

    let myScenario = scenario {
        do! UIScenario.SelectRootWindow "SimpleGUI"
        do! UIScenario.SelectControl "OK"

        return 1
        }

    let run = myScenario.Run ss

    0
