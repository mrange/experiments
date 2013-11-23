namespace mst

open System.Windows.Automation

[<AutoOpen>]
module Utils =
    
    type Query =
        |   ByName      of string
        |   ByClass     of string
        |   ById        of string
        |   ByProcessId of int
        |   Current

    let FindChild (q : Query) (ts : TreeScope) (elem : AutomationElement)= 
        let finder (e : AutomationElement) p (n : 'T) f = 
            let cond = PropertyCondition(p, n, f)
            e.FindFirst(ts, cond)

        match q with
        |   ByName      n   ->  finder elem AutomationElement.NameProperty          n   PropertyConditionFlags.IgnoreCase
        |   ByClass     n   ->  finder elem AutomationElement.ClassNameProperty     n   PropertyConditionFlags.IgnoreCase
        |   ById        n   ->  finder elem AutomationElement.AutomationIdProperty  n   PropertyConditionFlags.IgnoreCase
        |   ByProcessId n   ->  finder elem AutomationElement.ProcessIdProperty     n   PropertyConditionFlags.None
        |   Current         ->  elem

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

    let DefaultOf<'T> = Unchecked.defaultof<'T>
                

