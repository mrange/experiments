namespace mst

open System.Windows.Automation

[<AutoOpen>]
module Utils =
    
    let FindChild (name : string) (elem : AutomationElement) = 
        let cond = PropertyCondition(AutomationElement.AutomationIdProperty, name, PropertyConditionFlags.IgnoreCase)
        elem.FindFirst(TreeScope.Children, cond)

    let Slice (keep : int) (l : 'T list) = 
        if keep >= l.Length then
            [||],l
        else
            let res = Array.zeroCreate (l.Length - keep)
            let mutable ll = l
            for i in 0..(res.Length - 1) do
                res.[i] <- ll.Head
                ll <- ll.Tail
            res,ll
                

