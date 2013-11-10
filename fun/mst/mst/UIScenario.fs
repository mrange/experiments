namespace mst

open System.Windows.Automation

module UIScenario =
    
    let State_Window    = "UISCENARIO_STATE_WINDOW"
    let State_Control   = "UISCENARIO_STATE_CONTROL"

    let SetRootElement (elementName : string) : Scenario<unit> =
        scenario {
                    do! Scenario.LiftStackFrame

                    let root = AutomationElement.RootElement |> FindChild elementName 
                    if root = null then
                        do! Scenario.Raise ("RootElement not found: " + elementName)
                    else
                        do! Scenario.SetVariable State_Window root
                    }

    let GetCurrentElement : Scenario<AutomationElement> = 
        scenario {
                    return! Scenario.GetVariable State_Window
                    }

    let FindElement (elementName : string) : Scenario<AutomationElement option> =     
        scenario {
                    let! currentElement = GetCurrentElement
                    let element = currentElement |> FindChild elementName
                    if element = null then
                        return None
                    else
                        return Some element                        
                    }

    let GetElement (elementName : string) : Scenario<AutomationElement> =     
        scenario {
                    let! element = FindElement elementName
                    let elem = 
                        match element with
                        | Some c    -> c
                        | _         -> null 
                    
                    if elem = null then
                        return! Scenario.Raise ("Element not found: " + elementName)
                    else
                        return elem
                    }

    let SelectElement (elementName : string) : Scenario<unit> =     
        scenario {
                    do! Scenario.LiftStackFrame

                    let! element = GetElement elementName
                    do! Scenario.SetVariable State_Control elementName
                    }

    let GetPattern (elementName : string) (p : AutomationPattern) : Scenario<#BasePattern> =     
        scenario {
                    let! element = GetElement elementName
                    let o = ref Unchecked.defaultof<obj>
                    if element.TryGetCurrentPattern(p, o) then
                        return downcast !o
                    else
                        return! Scenario.Raise ("Element with requested pattern not found: " + elementName)
                    }

    let GetInvokePattern (elementName : string) : Scenario<InvokePattern> =     
        scenario {
                    let! p = GetPattern elementName InvokePattern.Pattern
                    return p
                    }

    let GetTextPattern (elementName : string) : Scenario<TextPattern> =     
        scenario {
                    let! p = GetPattern elementName TextPattern.Pattern
                    return p
                    }

    let Invoke (elementName : string) : Scenario<unit> =     
        scenario {
                    let! pattern = GetInvokePattern elementName
                    ignore <| pattern.Invoke()
                    return ()
                    }

    let GetText (elementName : string) : Scenario<string> =     
        scenario {
                    let! pattern = GetTextPattern elementName
                    return pattern.DocumentRange.GetText(-1)
                    }

    let StartWindowedProcess exePath windowName = scenario {
        do! Scenario.LiftStackFrame

        do! ProcessScenario.StartProcess exePath
        do! Scenario.Retry 10 500 (SetRootElement windowName)
        }
    
