namespace mst

open System.Windows.Automation

[<AutoOpen>]
module Utils =
    
    let FindChild (name : string) (elem : AutomationElement) = 
        let cond = PropertyCondition(AutomationElement.AutomationIdProperty, name, PropertyConditionFlags.IgnoreCase)
        elem.FindFirst(TreeScope.Children, cond)

