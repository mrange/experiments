open mst

open System.Windows.Automation

[<EntryPoint>]
let main argv = 

    let myScenario = scenario {
        do! UIScenario.StartWindowedProcess "SimpleGUI.exe" "SimpleGUI"

        do! UIScenario.Invoke "OK"

        let! text = UIScenario.GetText "Input"

        return 1
        }

    let run = Scenario.Run Map.empty myScenario

    0
