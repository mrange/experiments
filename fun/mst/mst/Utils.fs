namespace mst

open System.Windows.Automation

[<AutoOpen>]
module Utils =
    
    type Query =
        |   ByName      of string
        |   ByClass     of string
        |   ById        of string

    let FindChild (q : Query) (ts : TreeScope) (elem : AutomationElement)= 
        let p,n = 
            match q with
            |   ByName  n   ->  AutomationElement.NameProperty          , n
            |   ByClass n   ->  AutomationElement.ClassNameProperty     , n
            |   ById    n   ->  AutomationElement.AutomationIdProperty  , n
        let cond = PropertyCondition(p, n, PropertyConditionFlags.IgnoreCase)
        elem.FindFirst(ts, cond)

    let ShallowFindChild (q : Query) (elem : AutomationElement)= 
        FindChild q TreeScope.Children elem

    let DeepFindChild (q : Query) (elem : AutomationElement)= 
        FindChild q TreeScope.Subtree elem

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
                

