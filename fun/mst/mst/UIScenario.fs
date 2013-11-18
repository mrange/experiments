namespace mst

open System
open System.Windows.Automation
open System.Windows

open mst.lowlevel

type MouseGesture =
    |   LeftClick           of Point
    |   LeftClickAndHold    of Point
    |   ReleaseLeft         of Point
    |   MoveTo              of Point

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
                        root.SetFocus()
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

    let FocusElement (q : Query) : Scenario<unit> =     
        scenario {
                    let! element = GetElement q
                    return element.SetFocus()
                    }

    let FindPropertyValue (q : Query) (ap : AutomationProperty) : Scenario<'T option> =     
        scenario {
                    let! element = GetElement q
                    return
                        match element.GetCurrentPropertyValue(ap) with
                        | :? 'T as v    -> Some v
                        | _             -> None
                    }

    let GetPropertyValue (q : Query) (ap : AutomationProperty) : Scenario<'T> =     
        scenario {
                    let! p = FindPropertyValue q ap
                    if p.IsSome then
                        return p.Value
                    else
                        return! Scenario.Raise (sprintf "Property not found: %A" ap)
                    }

    let GetBounds (q : Query) : Scenario<Rect> = GetPropertyValue q AutomationElementIdentifiers.BoundingRectangleProperty

    let GetPattern (q : Query) (p : AutomationPattern) : Scenario<#BasePattern> =     
        scenario {
                    let! element = GetElement q
                    let o = ref Unchecked.defaultof<obj>
                    if element.TryGetCurrentPattern(p, o) then
                        return downcast !o
                    else
                        return! Scenario.Raise (sprintf "Element with requested pattern not found: %A" q)
                    }

    let GetInvokePattern (q : Query) : Scenario<InvokePattern> = GetPattern q InvokePattern.Pattern    

    let GetTextPattern (q : Query) : Scenario<TextPattern> = GetPattern q TextPattern.Pattern        

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

    let SendText (s : string) : Scenario<unit> =
        scenario {
                    ignore <| Keyboard.Send (s)
                    
                    return ()
                    }

    let SendChar (ch : char) (m : Modifier) : Scenario<unit> =
        scenario {
                    ignore <| Keyboard.Send (ch, m)
                    
                    return ()
                    }

    let DoMouseGesture (gs : MouseGesture list) : Scenario<unit> =
        scenario {
                    let round (v : float) = int <| Math.Round(v)
                    let apply (gestures : MouseGesture list) = 
                        let mutable p       = Point()
                        let mutable left    = false

                        for g in gestures do
                            match g with
                            | LeftClick pp          -> left <- false
                                                       p <- pp
                                                       ignore <| Mouse.LeftClick(round p.X, round p.Y)
                            | LeftClickAndHold pp   -> left <- true
                                                       p <- pp
                                                       ignore <| Mouse.LeftClickAndHold(round p.X, round p.Y)
                            | ReleaseLeft pp        -> left <- false
                                                       p <- pp
                                                       ignore <| Mouse.ReleaseLeft(round p.X, round p.Y)
                            | MoveTo pp             -> p <- pp
                                                       ignore <| Mouse.MoveTo(round p.X, round p.Y)

                        if (left) then ignore <| Mouse.ReleaseLeft(round p.X, round p.Y)


                    return apply gs
                    }

    let StartWindowedProcess exePath = scenario {
        do! Scenario.LiftStackFrame

        let! pid = ProcessScenario.StartProcess exePath
        do! Scenario.Retry 10 500 (SetRootElement <| ByProcessId pid)
        }
    
