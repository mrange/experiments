namespace mst

open System.Windows.Automation

module UIScenario =
    
    let State_Window    = "UISCENARIO_STATE_WINDOW"
    let State_Control   = "UISCENARIO_STATE_CONTROL"

    let SelectRootWindow (windowName : string) : Scenario<unit> =
        scenario {
                    let root = AutomationElement.RootElement |> FindChild windowName 
                    if root = null then
                        do! Scenario.SetState State_Window root
                    else
                        do! Scenario.Raise ("Window not found: " + windowName)
                    }

    let GetRootWindow : Scenario<AutomationElement> = 
        scenario {
                    let! elem = Scenario.GetState State_Window 
                    return elem
                    }

    let SelectControl (controlName : string) : Scenario<unit> =     
        scenario {
                    let! rootWindow = GetRootWindow
                    let control = rootWindow |> FindChild controlName
                    if control = null then
                        do! Scenario.SetState State_Control control
                    else
                        do! Scenario.Raise ("Control not found: " + controlName)
                    }
