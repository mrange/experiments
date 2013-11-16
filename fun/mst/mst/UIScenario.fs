namespace mst

open System.Windows.Automation

module UIScenario =
    
    let State_Window    = "UISCENARIO_STATE_WINDOW"
    let State_Current   = "UISCENARIO_STATE_CURRENT"

    let SetRootElement (q : Query) : Scenario<unit> =
        scenario {
                    do! Scenario.LiftStackFrame

                    let root = AutomationElement.RootElement |> ShallowFindChild q
                    if root = null then
                        do! Scenario.Raise (sprintf "RootElement not found: %A" q)
                    else
                        do! Scenario.SetVariable State_Window root
                        do! Scenario.SetVariable State_Current root
                    }

    let GetCurrentElement : Scenario<AutomationElement> = 
        scenario {
                    return! Scenario.GetVariable State_Current
                    }

    let FindElement (q : Query) : Scenario<AutomationElement option> =     
        scenario {
                    let! currentElement = GetCurrentElement
                    let element = currentElement |> DeepFindChild q 
                    if element = null then
                        return None
                    else
                        return Some element                        
                    }

    let GetElement (q : Query) : Scenario<AutomationElement> =     
        scenario {
                    let! element = FindElement q
                    let elem = 
                        match element with
                        | Some c    -> c
                        | _         -> null 
                    
                    if elem = null then
                        return! Scenario.Raise (sprintf "Element not found: %A" q)
                    else
                        return elem
                    }

    let GetRootElement : Scenario<AutomationElement> = 
        scenario {
                    return! Scenario.GetVariable State_Window
                    }

    let SetCurrentElement (q : Query) : Scenario<unit> =     
        scenario {
                    do! Scenario.LiftStackFrame

                    let! element = GetElement q
                    do! Scenario.SetVariable State_Current element
                    }

    let GetPattern (q : Query) (p : AutomationPattern) : Scenario<#BasePattern> =     
        scenario {
                    let! element = GetElement q
                    let o = ref Unchecked.defaultof<obj>
                    if element.TryGetCurrentPattern(p, o) then
                        return downcast !o
                    else
                        return! Scenario.Raise (sprintf "Element with requested pattern not found: %A" q)
                    }

    let GetInvokePattern (q : Query) : Scenario<InvokePattern> =     
        scenario {
                    let! p = GetPattern q InvokePattern.Pattern
                    return p
                    }

    let GetTextPattern (q : Query) : Scenario<TextPattern> =     
        scenario {
                    let! p = GetPattern q TextPattern.Pattern
                    return p
                    }

    let Invoke (q : Query) : Scenario<unit> =     
        scenario {
                    let! pattern = GetInvokePattern q
                    ignore <| pattern.Invoke()
                    return ()
                    }

    let GetText (q : Query) : Scenario<string> =     
        scenario {
                    let! pattern = GetTextPattern q
                    return pattern.DocumentRange.GetText(-1)
                    }

    let StartWindowedProcess exePath windowName = scenario {
        do! Scenario.LiftStackFrame

        do! ProcessScenario.StartProcess exePath
        do! Scenario.Retry 10 500 (SetRootElement windowName)
        }
    
