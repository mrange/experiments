namespace FolderSize

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.InteropServices

open SharpDX

open Units

module Logical = 


    [<StructuralEquality>]
    [<StructuralComparison>]
    type LayoutRotation = 
        | D0
        | D90
        | D180
        | D270

    [<StructuralEquality>]
    [<StructuralComparison>]
    type LayoutTransform = 
        {
            Rotation : LayoutRotation
            Scaling  : float32
        }

    [<NoEquality>]
    [<NoComparison>]
    type PropertyDefaultValue<'T> =
        | Value         of 'T
        | ValueCreator  of (Element -> 'T)
    and PropertyValueChanged<'T> = Element -> 'T -> 'T -> unit
    and [<AbstractClass>] Property(id : string, ``type`` : Type, declaringType : Type) = 

        static let __NoAction              (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()

        member x.Id             = id
        member x.Type           = ``type``
        member x.DeclaringType  = declaringType
        member x.IsEmpty                                = x.Equals (Property.Empty)

        static member Value v = fun () -> v
        static member Create declaringType id valueChanged valueCreator = Property<'T>(id,declaringType,valueCreator,valueChanged)
        static member Empty = Property.Create typeof<Property> "<EMPTY>" __NoAction <| Value (obj())


    and Property<'T>(id : string, declaringType : Type, value : PropertyDefaultValue<'T>, valueChanged : PropertyValueChanged<'T>)= 
        inherit Property(id, typeof<'T>, declaringType)    

        member x.DefaultValue (e : Element)             = 
                    match value with
                    | Value         v -> v      , true
                    | ValueCreator  v -> v e    , false
        member x.ValueChanged le oldValue newValue      = 
                    valueChanged le oldValue newValue

        member x.Value (v : 'T) = PropertyValue<'T>(x, v)

    and [<AbstractClass>] PropertyValue(p : Property)= 
        
        abstract OnGetValue : unit -> obj

        member x.Property   = p
        member x.Value      = x.OnGetValue ()

    and PropertyValue<'T>(p : Property, v : 'T)= 
        inherit PropertyValue(p)
        
        override x.OnGetValue ()    = v :> obj

        member x.TypedValue         = v

    and Element() = 
        
        let mutable parent : Element option = None

        static let __NoAction              (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()
        static let __InvalidateMeasurement (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateMeasurement   ()
        static let __InvalidatePlacement   (le : Element) (ov : 'T) (nv : 'T) = le.InvalidatePlacement     ()
        static let __InvalidateVisual      (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateVisual        ()

        static let children : Element array = [||]
        let properties = Dictionary<Property, obj>()

        static let Create id valueChanged value = Property.Create typeof<Element> id valueChanged value 

        static member Measurement       = Create "Measurement"     __NoAction              <| Value (None : Measurement option)
        static member Placement         = Create "Placement"       __NoAction              <| Value (None : Placement option)
        static member Visual            = Create "Visual"          __NoAction              <| Value (None : VisualTree option)
                                                                                           
        static member Bounds            = Create "Bounds"          __InvalidateMeasurement <| Value Bounds.MinMin
        static member IsVisible         = Create "IsVisible"       __InvalidateMeasurement <| Value true           
                                                                                           
        static member Margin            = Create "Margin"          __InvalidateMeasurement <| Value Thickness.Zero 
                                                                                           
        static member FontFamily        = Create "FontFamily"      __InvalidateMeasurement <| Value "Verdana"      
        static member FontSize          = Create "FontSize"        __InvalidateMeasurement <| Value 12.F           
                                                                                           
                                                                                           
        static member BackgroundBrush   = Create "BackgroundBrush" __InvalidateVisual      <| Value BrushDescriptor.Transparent

        abstract OnChildren     : unit -> Element array
        default x.OnChildren () = children
        member x.Children       = x.OnChildren ()

        member x.Parent 
            with get ()         = parent

        member internal x.SetParent p =
                            match parent with
                            | None      -> ()
                            | Some pp   -> failwith "Element is already a member of a logical tree"
                            parent <- Some p
                            match parent with
                            | None      -> ()
                            | Some pp   -> pp.InvalidateMeasurement ()

        member internal x.ClearParent () =
                            match parent with
                            | None      -> ()
                            | Some pp   -> pp.InvalidateMeasurement ()
                            parent <- None

        member private x.ValidateProperty (lp :Property<'T>) =
            let t = x.GetType ()
            if not <| lp.DeclaringType.IsAssignableFrom t then
                failwithf "Property %s.%s is not a member %s" lp.DeclaringType.Name lp.Id t.Name

        member private x.TryGet (lp :Property<'T>)  : 'T option = 
                let v = properties.Find lp
                match v with
                | None      -> None
                | Some v    -> 
                    let tv = v.As<'T> ()
                    match tv with
                    | None      -> Debug.Assert false; None
                    | Some tv   -> Some tv


        member x.Get    (lp :Property<'T>)           : 'T = 
                x.ValidateProperty lp
                let v = x.TryGet lp
                match v with
                | Some v    -> v
                | None      -> 
                    ignore <| properties.Remove lp  // Shouldn't be necessary but if the TryGet assert fails this is required to clear local value
                    let dv,shared = lp.DefaultValue x
                    if not shared then 
                        properties.Add(lp,dv)
                    dv  // No ValueChanged on initializing the default value
        member x.Set    (lp :Property<'T>) (v : 'T)  : unit = 
                x.ValidateProperty lp
                let pv = x.Get lp
                if pv.Equals v then ()
                else
                    properties.[lp] <- v
                    lp.ValueChanged x pv v
        member x.Clear  (lp :Property<'T>)           : unit = 
                x.ValidateProperty lp
                let v = x.TryGet lp
                ignore <| properties.Remove lp  // Shouldn't be necessary but if the TryGet assert fails this is required to clear local value
                match v with
                | None      -> ()
                | Some v   ->
                    // Property value found, reset to default value and raise ValueChanged
                    let dv,shared = lp.DefaultValue x
                    if not shared then
                        properties.Add(lp,dv)
                    lp.ValueChanged x v dv

        member x.NoAction               () = ()            
        member x.InvalidateMeasurement  () = 
            let m = x.Get Element.Measurement
            match m with 
            | None      -> ()
            | Some _    -> x.Clear Element.Measurement
                           x.Clear Element.Placement
                           x.Clear Element.Visual
                           match x.Parent with
                           | Some p -> p.InvalidateMeasurement ()          
                           | None   -> ()
        member x.InvalidatePlacement    () =             
            let p = x.Get Element.Placement
            match p with 
            | None      -> ()
            | Some _    -> x.Clear Element.Placement
                           x.Clear Element.Visual
                           match x.Parent with
                           | Some p -> p.InvalidatePlacement ()          
                           | None   -> ()
        member x.InvalidateVisual       () = 
            let v = x.Get Element.Visual
            match v with 
            | None      -> ()
            | Some _    -> x.Clear Element.Visual
                           match x.Parent with
                           | Some p -> p.InvalidateVisual ()          
                           | None   -> ()

        abstract OnGetBox                           : unit -> Thickness
        default x.OnGetBox ()                       = x.Get Element.Margin

        member x.Box                                = x.OnGetBox ()

        abstract OnMeasureContent                   : Available -> Measurement
        default x.OnMeasureContent m                = Measurement.Fill

        member x.MeasureElement (a  : Available)    = 
                    let cachedMeasure = x.Get Element.Measurement
                    match cachedMeasure with
                    | Some m when a.IsMeasurementValid m  -> m
                    | _                                   -> 
                        let box = x.Box
                        let bounds = x.Get Element.Bounds
                        let innerMeasure = x.OnMeasureContent<| a - box
                        let finalMeasure = bounds.AdjustMeasurement a (innerMeasure + box)
                        x.Set Element.Measurement <| Some finalMeasure
                        x.Set Element.Placement None
                        x.Set Element.Visual None
                        finalMeasure

        abstract OnPlaceContent                     : Placement -> unit
        default x.OnPlaceContent p                  = ()

        member x.PlaceElement   (p : Placement)     = 
                    let cachedPlacement = x.Get Element.Placement
                    match cachedPlacement with
                    | Some cp when cp = p -> ()
                    | _                   -> 
                        let cachedMeasure = x.Get Element.Measurement
                        match cachedMeasure with
                        | None            -> ()
                        | Some cm         ->  
                            let box = x.Box
                            let bounds = x.Get Element.Bounds
                            let finalPlacement = bounds.AdjustPlacement cm p
                            x.OnPlaceContent <| finalPlacement - box
                            x.Set Element.Placement <| Some finalPlacement
                            x.Set Element.Visual None
                                                        

        abstract OnRenderContent                    : Placement -> Placement -> VisualTree
        default x.OnRenderContent   (o : Placement)
                                    (i : Placement)
                                                    = VisualTree.NoVisual
        member x.RenderContent  ()                  = 
                    let box = x.Box
                    let p = x.Get Element.Placement
                    match p with
                    | Some pp -> x.OnRenderOverlay pp <| pp - box
                    | None    -> NoVisual

        abstract OnRenderOverlay                    : Placement -> Placement -> VisualTree
        default x.OnRenderOverlay   (o : Placement)
                                    (i : Placement)
                                                    = VisualTree.NoVisual
        member x.RenderOverlay  ()                  = 
                    let box = x.Box
                    let p = x.Get Element.Placement
                    match p with
                    | Some pp -> x.OnRenderOverlay pp <| pp - box
                    | None    -> NoVisual

        member x.Render ()                          = 
                    let cachedVisual = x.Get Element.Visual
                    match cachedVisual with
                    | Some v    -> v
                    | None      -> 
                        let visualContent = x.RenderContent ()

                        // A bit of trickery to avoid allocation and shuffling of extra arrays
                        let children = x.Children
                        let visualChildren = Array.create (children.Length + 2) VisualTree.NoVisual

                        for i in 0..children.Length-1 do
                            visualChildren.[i + 1] <- children.[i].Render ()

                        let visualOverlay = x.RenderOverlay ()

                        visualChildren.[0] <- visualContent
                        visualChildren.[visualChildren.Length - 1] <- visualOverlay

                        let visual = 
                            match visualContent, children.Length, visualOverlay with
                            | VisualTree.NoVisual , 0     , VisualTree.NoVisual   -> VisualTree.NoVisual
                            | _                   , 0     , VisualTree.NoVisual   -> visualContent
                            | VisualTree.NoVisual , 0     , _                     -> visualOverlay
                            | VisualTree.NoVisual , 1     , VisualTree.NoVisual   -> visualChildren.[1] // The first visual child is located @ 1
                            | _                   , _     , _                     -> VisualTree.Group visualChildren

                        x.Set Element.Visual <| Some visual
                        visual
                                                      


                                                        
    let NoAction                (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()
    let InvalidateMeasurement   (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateMeasurement   ()
    let InvalidatePlacement     (le : Element) (ov : 'T) (nv : 'T) = le.InvalidatePlacement     ()
    let InvalidateVisual        (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateVisual        ()

    type Container() = 
        inherit Element()
    
        static let Create id valueChanged value = Property.Create typeof<Container> id valueChanged value 

        static member Padding           = Create "Padding"         InvalidateMeasurement    <| Value Thickness.Zero

        override x.OnGetBox ()          = x.Get Element.Margin + x.Get Container.Padding

    type Decorator() =
        inherit Container()

        let mutable child : Element option = None

        member x.Child 
            with get () =   child
            and set (c : Element option)   =   
                    match child with
                    | None          -> ()
                    | Some child    -> child.ClearParent () // Invalidates old parent

                    match c with
                    | None          -> ()
                    | Some child    -> child.SetParent x    // Invalidates parent

                    child <- c

    type Layout() = 
        inherit Container()
    
        let children    = SortedDictionary<int, Element>()

        let mutable cachedChildren = None

        static let Create id valueChanged value = Property.Create typeof<Container> id valueChanged value 

        override x.OnChildren ()    =   match cachedChildren with
                                        | Some c    ->  c
                                        | None      ->  let c = children |> Seq.map (fun kv -> kv.Value) |> Seq.toArray
                                                        cachedChildren <- Some c
                                                        c


        member x.InsertChild i le = ignore <| (children.[i] = le)
                                    le.SetParent x  // Invalidates parent
                                    cachedChildren <- None
                                    x

        member x.RemoveChild i =    let c = children.Find i
                                    match c with
                                    | None      -> ()
                                    | Some le    -> 
                                        ignore <| children.Remove i
                                        le.ClearParent ()   // Invalidates old parent
                                        cachedChildren <- None
                                    x  

        override x.OnGetBox ()          = x.Get Element.Margin + x.Get Container.Padding

        override x.OnMeasureContent m   = let children = x.Children
                                          let sz : Measurement option = None
                                          for c in children do
                                            ()
                                          Measurement.Zero

    type Document() =
        inherit Decorator()


    type Text() =
        inherit Element()

        static let Create id valueChanged value = Property.Create typeof<Text> id valueChanged value 

        static member Text          = Create "Text"        InvalidateMeasurement  (Value ""              )

    type Div() = 
        inherit Container ()

    module Elements = 
        let Text (ps : PropertyValue list) : Text  = Text()

        let Div (ps : PropertyValue list) (children : Element list)  = ()

        let body = 
            Div [
                    Element.Bounds.Value        Bounds.MinMax
                    Element.FontFamily.Value    ""
                ]
                [
                    Text []
                    Text []
                    Text []
                ]

