#r "UIAutomationTypes"
#r "UIAutomationClient"

open System
open System.Windows.Automation

let Patterns : Map<string, AutomationPattern>= 
    [
        typeof<TextPattern>         , TextPattern.Pattern    
        typeof<WindowPattern>       , WindowPattern.Pattern    
    ]
    |> List.fold (fun s (k,v) -> s |> Map.add k.FullName v) Map.empty

let QueryPattern (element : AutomationElement) (query : 'BasePattern -> 'T) (defaultValue : 'T) = 
    let typeName = typeof<'BasePattern>.FullName 
    let mp = Patterns |> Map.tryFind typeName
    match mp with
    |   Some p  ->  let o = ref Unchecked.defaultof<obj>
                    if element.TryGetCurrentPattern(p, o) then
                        match !o with
                        | :? 'BasePattern as bp -> query(bp)
                        | _                     -> 
                                                    defaultValue 
                    else
                        defaultValue
    |   _       ->  defaultValue

type UIElement(element : AutomationElement) = 

    let pattern query defaultValue = QueryPattern element query defaultValue 
    let property (dv : 'T) (p : AutomationProperty) =   
        let pv = element.GetCurrentPropertyValue p
        match pv with
        | :? 'T as t    -> t
        | _             -> dv

    member x.Element = element

    member x.AutomationId   = property "" AutomationElementIdentifiers.AutomationIdProperty
    member x.ClassName      = property "" AutomationElementIdentifiers.ClassNameProperty
    member x.Name           = property "" AutomationElementIdentifiers.NameProperty
    member x.WaitForInputIdle () = pattern (fun (p : WindowPattern) -> p.WaitForInputIdle(1000)) false

    member x.HasChildren = element.FindFirst(TreeScope.Children, Condition.TrueCondition) <> null

    member x.Children () =   
        [| for element in element.FindAll(TreeScope.Children, Condition.TrueCondition) do yield UIElement(element) |]

    override x.ToString () = x.ClassName + "::" + x.Name

let AllRootWindows () = UIElement(AutomationElement.RootElement).Children()

let FindFromName (partialName : string) (elements : UIElement array) = elements |> Array.filter (fun e -> e.Name.Contains(partialName))

type Tree = 
    |   Node of UIElement*Tree array

let rec DumpTree (element : UIElement) : Tree = 
    let subElements = element.Children() |> Array.map DumpTree
    Node (element,subElements)
    
                        
let all = AllRootWindows ()

let mspaint = (all |> FindFromName "Untitled").[0]
let tree = DumpTree mspaint