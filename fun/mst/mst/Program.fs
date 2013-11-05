open mst

open System.Windows.Automation

[<EntryPoint>]
let main argv = 

    let cond = PropertyCondition(AutomationElement.AutomationIdProperty, "SimpleGUI", PropertyConditionFlags.IgnoreCase)

    let x = AutomationElement.RootElement.FindFirst(TreeScope.Children, cond)

    0
