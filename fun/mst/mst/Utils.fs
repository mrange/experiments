namespace mst

open System.Windows.Automation

[<AutoOpen>]
module Utils =
    
    let FindChild (name : string) (elem : AutomationElement) = 
        let cond = PropertyCondition(AutomationElement.AutomationIdProperty, name, PropertyConditionFlags.IgnoreCase)
        elem.FindFirst(TreeScope.Children, cond)

    let FindChildByClassName (name : string) (elem : AutomationElement) = 
        let cond = PropertyCondition(AutomationElement.ClassNameProperty, name, PropertyConditionFlags.IgnoreCase)
        elem.FindFirst(TreeScope.Children, cond)

    let Slice (keep : int) (l : 'T list) = 
        if keep >= l.Length then
            [],l
        else
            let mutable res = []
            let mutable ll = l
            for i in 0..(l.Length - keep - 1) do
                res <- ll.Head::res
                ll <- ll.Tail
            (res |> List.rev),ll
                

