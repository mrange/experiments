#r "UIAutomationTypes"
#r "UIAutomationClient"
#r "WindowsBase"

open System
open System.Windows
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

    member x.AllProperties  = let props = element.GetSupportedProperties()
                              props 
                                |> Array.map (fun p -> let pv = element.GetCurrentPropertyValue p
                                                       p.ProgrammaticName, pv
                                                )

    member x.AutomationId   = property ""       AutomationElementIdentifiers.AutomationIdProperty
    member x.ClassName      = property ""       AutomationElementIdentifiers.ClassNameProperty
    member x.Name           = property ""       AutomationElementIdentifiers.NameProperty
    member x.BoundingRect   = property (Rect()) AutomationElementIdentifiers.BoundingRectangleProperty

    member x.WaitForInputIdle () = pattern (fun (p : WindowPattern) -> p.WaitForInputIdle(1000)) false

    member x.HasChildren = element.FindFirst(TreeScope.Children, Condition.TrueCondition) <> null

    member x.Children () =   
        [| for element in element.FindAll(TreeScope.Children, Condition.TrueCondition) do yield UIElement(element) |]
    
    member x.AsString = 
        let prettify s whenNull = if String.IsNullOrWhiteSpace(s) then whenNull else s
        lazy    let r = x.BoundingRect
                (sprintf "%s::%s (%s) - %.0f,%.0f %.0fx%.0f" (prettify x.ClassName "<NoClass>") (prettify x.Name "<NoName>") (prettify x.AutomationId  "<NoId>") r.Left r.Top r.Width r.Height)

    override x.ToString () = x.AsString.Value

let AllRootWindows () = UIElement(AutomationElement.RootElement).Children()

let FindFromName (partialName : string) (elements : UIElement array) = elements |> Array.filter (fun e -> e.Name.Contains(partialName))

type Tree = 
    |   Node of UIElement*Tree array

let Element t =   
    match t with
    |   Node (e,_) -> e

let Children t =   
    match t with
    |   Node (_, c) -> c

let rec DumpTree (element : UIElement) : Tree = 
    let subElements = element.Children() |> Array.map DumpTree
    Node (element,subElements)

let waitForAWhile = System.Threading.Thread.Sleep (2000)
                        
let all = AllRootWindows ()

let mspaint = (all |> FindFromName "Untitled").[0]
let tree = DumpTree mspaint
