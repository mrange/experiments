namespace silberman

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics

open SharpDX

open Fundamental
open Visual

module public Logical = 

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

    type StackOrientation = 
        | FromLeft
        | FromRight
        | FromTop
        | FromBottom

    let TransparentBrush        = BrushDescriptor.Transparent
    let SolidBrush (c : Color)  = BrushDescriptor.SolidColor <| ColorDescriptor.Color c

    module Foundation = 
        type ElementContext = 
            {
                MeasureText : TextFormatDescriptor -> Size2F -> string -> Size2F
            }
            static member New mt = {MeasureText = mt}

        type TypeDictionary<'T>() =

            let safe     = obj()
            let explicit = ConcurrentDictionary<Type,'T>()
            let implicit = ConcurrentDictionary<Type,'T option>()
            
            member x.Add k v = 
                lock safe <| fun () -> 
                                if explicit.TryAdd(k,v) then
                                    implicit.Clear ()
                                else 
                                    ()

            member x.Replace k (v : 'T) = 
                lock safe <| fun () -> 
                                ignore <| explicit.AddOrUpdate(k,v,(fun _ _ -> v))
                                implicit.Clear ()

            member x.Remove k v = 
                lock safe <| fun () -> 
                                if explicit.TryRemove(k,v) then
                                    implicit.Clear ()
                                else 
                                    ()

            member x.Clear () =
                lock safe <| fun () -> 
                                explicit.Clear ()
                                implicit.Clear ()

            member private x.TryFindBase_NoLock (k : Type) = 
                let v = RefOf<'T>
                let found = explicit.TryGetValue(k, v)
                if found then
                    let r = Some !v
                    ignore <| implicit.TryAdd(k,r)
                    r
                else
                    let b = k.BaseType
                    if b = null then None
                    else
                        let r = x.TryFindBase_NoLock b 
                        ignore <| implicit.TryAdd(k,r)
                        r


            member x.TryFind k = 
                let v : 'T option ref = ref None
                if implicit.TryGetValue(k, v) then
                    !v
                else
                    lock safe <| fun () -> x.TryFindBase_NoLock k
                        
                    

        [<NoEquality>]
        [<NoComparison>]
        type PropertyDefaultValue<'T> =
            | Value         of 'T
            | ValueCreator  of (Element -> 'T)
        and PropertyValueChanged<'T>    = Element -> 'T -> 'T -> unit
        and ComputePropertyValue<'T>    = Element -> 'T
        and EventHandler<'TEventValue>   = Element -> 'TEventValue -> bool
        and [<AbstractClass>] Member(id : string, declaringType : Type) = 

            member x.Id             = id
            member x.DeclaringType  = declaringType

            member x.IsMemberOf (t : Type) = declaringType.IsAssignableFrom t

            member x.ValidateMember (t : Type) =
                if not <| x.IsMemberOf t then
                    failwithf "%s %s.%s is not a member %s" (x.GetType().Name) x.DeclaringType.Name x.Id t.Name

        and [<AbstractClass>] Event(id : string, eventValueType : Type, declaringType : Type) =
            inherit Member(id, declaringType)

            member x.EventValueType = eventValueType
            
            static member Routed<'TDeclaring, 'TEventValue> id  (sample : 'TEventValue) = Event<'TEventValue>(id,typeof<'TDeclaring>)
            static member Empty = Event.Routed<Event, obj> "<EMPTY>"
            
        and [<Sealed>] Event<'TEventValue>(id : string, declaringType : Type) =
            inherit Event(id, typeof<'TEventValue>, declaringType)

            member x.Handler (eh : EventHandler<'TEventValue>) = EventListener<'TEventValue>(x, eh)

        and [<AbstractClass>] Property(id : string, ``type`` : Type, declaringType : Type) = 
            inherit Member(id, declaringType)

            static let __NoAction              (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()

            member x.Type           = ``type``
            member x.IsEmpty        = x.Equals (Property.Empty)

            abstract OnIsComputed   : unit -> bool
            member x.IsComputed     = x.OnIsComputed ()

            abstract OnIsPersistent : unit -> bool
            member x.IsPersistent   = x.OnIsPersistent ()

            static member Value v = fun () -> v

            static member Persistent<'TDeclaring, 'T when 'T : equality> id valueChanged valueCreator = PersistentProperty<'T>(id,typeof<'TDeclaring>,valueCreator,valueChanged)
            static member Computed<'TDeclaring, 'T>   id computeValue = ComputedProperty<'T>(id,typeof<'TDeclaring>,computeValue)

            static member Empty = Property.Computed<Property, _> "<EMPTY>" <| fun le -> obj()


        and [<AbstractClass>] Property<'T>(id : string, declaringType : Type) = 
            inherit Property(id, typeof<'T>, declaringType)    

        and [<Sealed>] PersistentProperty<'T when 'T : equality>(id : string, declaringType : Type, defaultValue : PropertyDefaultValue<'T>, valueChanged : PropertyValueChanged<'T>)= 
            inherit Property<'T>(id, declaringType)    

            static let overrideDefaultValue = TypeDictionary<PropertyDefaultValue<'T>>()
            static let overrideValueChanged = TypeDictionary<PropertyValueChanged<'T>>()

            override x.OnIsComputed ()      = false
            override x.OnIsPersistent ()    = true

            member x.DefaultValue (e : Element)             = 
                        let dv = (overrideDefaultValue.TryFind <| e.GetType()) <??> defaultValue
                        match dv with
                        | Value         v -> v      , true
                        | ValueCreator  vc-> vc e   , false

            member x.ValueChanged e oldValue newValue      = 
                        let vc = (overrideValueChanged.TryFind <| e.GetType()) <??> valueChanged
                        vc e oldValue newValue
                        

            member x.Value (v : 'T) = PropertyValue<'T>(x, v)

            member x.Override<'TOverride> (defaultValue : PropertyDefaultValue<'T> option) (valueChanged : PropertyValueChanged<'T> option) =
                        let overrideType = typeof<'TOverride>
                        x.ValidateMember overrideType
                        match defaultValue with
                        | Some defaultValue -> overrideDefaultValue.Replace overrideType defaultValue
                        | None              -> ()
                        match valueChanged with
                        | Some valueChanged -> overrideValueChanged.Replace overrideType valueChanged
                        | None              -> ()
                        ()

        and [<Sealed>] ComputedProperty<'T>(id : string, declaringType : Type, computeValue : ComputePropertyValue<'T>) = 
            inherit Property<'T>(id, declaringType)

            static let overrideCompute = TypeDictionary<ComputePropertyValue<'T>>()

            override x.OnIsComputed ()      = true
            override x.OnIsPersistent ()    = false

            member x.ComputeValue (e : Element) = 
                        let cv = (overrideCompute.TryFind <| e.GetType()) <??> computeValue
                        cv e

            member x.Override<'TOverride> (computeValue : ComputePropertyValue<'T> option) =
                        let overrideType = typeof<'TOverride>
                        x.ValidateMember overrideType
                        match computeValue with
                        | Some computeValue -> overrideCompute.Replace overrideType computeValue
                        | None              -> ()
                        ()

        and [<AbstractClass>] Element() = 
        
            let mutable parent  : Element option        = None
            let mutable context : ElementContext option = None

            static let __NoAction              (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()
            static let __InvalidateMeasurement (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateMeasurement   ()
            static let __InvalidatePlacement   (le : Element) (ov : 'T) (nv : 'T) = le.InvalidatePlacement     ()
            static let __InvalidateVisual      (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateVisual        ()

            static let Persistent id valueChanged value = Property.Persistent<Element, _>   id valueChanged value 
            static let Computed   id computeValue       = Property.Computed<Element, _>     id computeValue

            static let children : Element array = [||]
            let properties      = Dictionary<Property, obj>()
            let eventHandlers   = Dictionary<Event, obj>()

            abstract OnChildren     : unit -> Element array

            abstract OnMeasureContent                   : Available -> Measurement

            abstract OnGetEffectiveMargin               : unit -> Thickness

            abstract OnPlaceElement                     : Placement -> Placement -> unit

            abstract OnRenderContent                    : Placement -> Placement -> VisualTree
            abstract OnRenderOverlay                    : Placement -> Placement -> VisualTree

            abstract OnRenderChild                      : Placement -> Placement -> Element -> VisualTree

            member x.Children       = x.OnChildren ()

            member x.Parent 
                with get ()         = parent

            member x.Context        = context

            member internal x.SetContext ctx = context <- Some ctx
            member internal x.SetParent p =
                                match parent with
                                | None      -> ()
                                | Some pp   -> failwith "Element is already a member of a logical tree"
                                parent <- Some p
                                context <- p.Context
                                match parent with
                                | None      -> ()
                                | Some pp   -> pp.InvalidateMeasurement ()

            member internal x.ClearParent () =
                                match parent with
                                | None      -> ()
                                | Some pp   -> pp.InvalidateMeasurement ()
                                parent <- None
                                context <- None

            member private x.ValidateProperty (lp :Property<'T>) =
                lp.ValidateMember <| x.GetType()

            member private x.ValidateEvent (e :Event<'T>) =
                e.ValidateMember <| x.GetType()

            member private x.TryGet (lp :Property<'T>)  : 'T option = 
                    let v = properties.Find lp
                    match v with
                    | None      -> None
                    | Some v    -> 
                        let tv = v.As<'T> ()
                        match tv with
                        | None      -> Debug.Assert false; None
                        | Some tv   -> Some tv


            member x.Get    (lp : ComputedProperty<'T>)  : 'T = 
                    x.ValidateProperty lp
                    lp.ComputeValue x

            member x.Get    (lp : PersistentProperty<'T>)  : 'T = 
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

            member x.Get    (lp : Property<'T>)           : 'T = 
                    if lp.IsComputed then
                        x.Get (lp :?> ComputedProperty<'T>)
                    else
                        x.Get (lp :?> PersistentProperty<'T>)

            member x.Set<'T when 'T : equality> (lp : PersistentProperty<'T>) (v : 'T)  : unit = 
                    x.ValidateProperty lp
                    let pv = x.Get lp
                    if pv = v then ()
                    else
                        properties.[lp] <- v
                        lp.ValueChanged x pv v
            member x.Clear  (lp : PersistentProperty<'T>)           : unit = 
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

            member x.AssignFromPropertyValues (pvs : PropertyValue list) =
                for pv in pvs do
                    ignore <| pv.AssignValueTo x 

            member internal x.RaiseEventImpl<'TEventValue> (e : Event<'TEventValue>) (v : 'TEventValue) = 
                    x.ValidateEvent e
                    let event = eventHandlers.Find e
                    match event,parent with
                    | None      , None          -> false
                    | None      , Some parent   -> parent.RaiseEventImpl e v
                    | Some eh   , None          -> let eh : EventHandler<'TEventValue> = downcast eh
                                                   eh x v
                    | Some eh   , Some parent   -> let eh : EventHandler<'TEventValue> = downcast eh
                                                   let handled = eh x v
                                                   if handled then true
                                                   else parent.RaiseEventImpl e v

            member x.RaiseEvent<'TEventValue> (e : Event<'TEventValue>) (v : 'TEventValue) = 
                    x.ValidateEvent e
                    x.RaiseEventImpl e v

            member x.ClearEventHandler (e : Event<'TEventValue>) = 
                    ignore <| eventHandlers.Remove e

            member x.SetEventHandler (e : Event<'TEventValue>) (eh : EventHandler<'TEventValue>) = 
                    eventHandlers.[e] <- eh

            static member Measurement           = Persistent "Measurement"     __NoAction              <| Value (None : Measurement option)
            static member Placement             = Persistent "Placement"       __NoAction              <| Value (None : Placement option)
            static member Visual                = Persistent "Visual"          __NoAction              <| Value (None : VisualTree option)
                                                                                               
            static member Bounds                = Persistent "Bounds"          __InvalidateMeasurement <| Value Bounds.MinMin
            static member IsVisible             = Persistent "IsVisible"       __InvalidateMeasurement <| Value true           
                                              
            static member Margin                = Persistent "Margin"          __InvalidateMeasurement <| Value Thickness.Zero 
                                              
            static member FontFamily            = Persistent "FontFamily"      __InvalidateMeasurement <| Value "Verdana"      
            static member FontSize              = Persistent "FontSize"        __InvalidateMeasurement <| Value 12.F           

            static member Background            = Persistent "Background"       __InvalidateVisual      <| Value BrushDescriptor.Transparent
            static member Foreground            = Persistent "Foreground"       __InvalidateVisual      <| Value (SolidBrush Color.Black)

            static member TextFormatDescriptor  = Computed   "FontSize"        <| fun x -> 
                                                                                    let fontFamily  = x.Get Element.FontFamily
                                                                                    let fontSize    = x.Get Element.FontSize
                                                                                    TextFormatDescriptor.New fontFamily fontSize

            default x.OnChildren () = children
            default x.OnMeasureContent m                = Measurement.Fill
            default x.OnGetEffectiveMargin ()           = x.Get Element.Margin
            default x.OnPlaceElement    (o : Placement)
                                        (i : Placement)
                                                        = ()
            default x.OnRenderContent   (o : Placement)
                                        (i : Placement)
                                                        = VisualTree.NoVisual
            default x.OnRenderOverlay   (o : Placement)
                                        (i : Placement)
                                                        = VisualTree.NoVisual
            default x.OnRenderChild     (o : Placement)
                                        (i : Placement)
                                        (e : Element)
                                                        = e.Render ()        

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

            member x.EffectiveMargin                    = x.OnGetEffectiveMargin ()

            member x.MeasureElement (a  : Available)    = 
                        let cachedMeasure = x.Get Element.Measurement
                        match cachedMeasure with
                        | Some m when a.IsMeasurementValid m  -> m
                        | _                                   -> 
                            let box = x.EffectiveMargin
                            let bounds = x.Get Element.Bounds
                            let innerMeasure = x.OnMeasureContent<| a - box
                            let finalMeasure = bounds.AdjustMeasurement a (innerMeasure + box)
                            x.Set Element.Measurement <| Some finalMeasure
                            x.Set Element.Placement None
                            x.Set Element.Visual None
                            finalMeasure

            member x.PlaceElement   (p : Placement)     = 
                        let cachedPlacement = x.Get Element.Placement
                        match cachedPlacement with
                        | Some cp when cp = p -> ()
                        | _                   -> 
                            let cachedMeasure = x.Get Element.Measurement
                            match cachedMeasure with
                            | None            -> ()
                            | Some cm         ->  
                                let box = x.EffectiveMargin
                                let bounds = x.Get Element.Bounds
                                let finalPlacement = bounds.AdjustPlacement cm p
                                x.OnPlaceElement finalPlacement <| finalPlacement - box
                                x.Set Element.Placement <| Some finalPlacement
                                x.Set Element.Visual None
                                                        

            member x.Render ()                          = 
                        let cachedVisual = x.Get Element.Visual
                        match cachedVisual with
                        | Some v    -> v
                        | None      -> 
                            let box = x.EffectiveMargin
                            let p = x.Get Element.Placement
                            match p with
                            | None                      -> NoVisual
                            | Some p when p.IsZero      -> NoVisual
                            | Some outer                -> 
                            
                                let inner = outer - box

                                let visualContent = x.OnRenderContent outer inner

                                // A bit of trickery to avoid allocation and shuffling of extra arrays
                                let children = x.Children
                                let visualChildren = Array.create (children.Length + 2) VisualTree.NoVisual

                                for i in 0..children.Length-1 do
                                    visualChildren.[i + 1] <- x.OnRenderChild outer inner children.[i]

                                let visualOverlay = x.OnRenderOverlay outer inner

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
        and [<AbstractClass>] EventListener(e : Event) =  
        
            abstract OnSetListener  : Element -> unit
            abstract OnClearListener: Element -> unit

            member x.SetListener (ee : Element) = 
                        if e.IsMemberOf <| ee.GetType () then
                            x.OnSetListener ee 
                            true
                        else false

            member x.ClearListener (ee : Element) = 
                        if e.IsMemberOf <| ee.GetType () then
                            x.OnClearListener ee 
                            true
                        else false

            member x.Event  = e

        and EventListener<'TEventValue>(e : Event<'TEventValue>, handler : EventHandler<'TEventValue>)= 
            inherit EventListener(e)
        
            member x.Handler : EventHandler<'TEventValue> = handler

            override x.OnSetListener (ee : Element) = 
                        ee.SetEventHandler e handler
                        ()
                    
            override x.OnClearListener (ee : Element) = 
                        ee.ClearEventHandler e
                        ()
        and [<AbstractClass>] PropertyValue(p : Property) =  
        
            abstract OnAssignValueTo : Element -> bool

            member x.AssignValueTo (e : Element) = 
                        if p.IsMemberOf <| e.GetType () then
                            x.OnAssignValueTo e 
                        else false

            member x.Property   = p

        and PropertyValue<'T when 'T : equality>(p : PersistentProperty<'T>, v : 'T)= 
            inherit PropertyValue(p)
        
            member x.Value      = v

            override x.OnAssignValueTo (e : Element) = 
                        e.Set p v
                        true
                    

                                                      
    let NoAction                (e : Foundation.Element) (ov : 'T) (nv : 'T) = e.NoAction                ()
    let InvalidateMeasurement   (e : Foundation.Element) (ov : 'T) (nv : 'T) = e.InvalidateMeasurement   ()
    let InvalidatePlacement     (e : Foundation.Element) (ov : 'T) (nv : 'T) = e.InvalidatePlacement     ()
    let InvalidateVisual        (e : Foundation.Element) (ov : 'T) (nv : 'T) = e.InvalidateVisual        ()

    module Standard = 

        open Foundation

        type [<AbstractClass>] ContainerElement() = 
            inherit Element()
    
            static let Persistent id valueChanged value = Property.Persistent<ContainerElement, _>  id valueChanged value 

            static member Padding               = Persistent "Padding"         InvalidateMeasurement    <| Value Thickness.Zero

            override x.OnGetEffectiveMargin ()  = x.Get Element.Margin + x.Get ContainerElement.Padding

        type [<AbstractClass>] DecoratorElement() =
            inherit ContainerElement()

            static let InvalidateChild (e : Element) (ov : Element option) (nv : Element option) = 
                        match ov with
                        | None          -> ()
                        | Some child    -> child.ClearParent () // Invalidates old parent

                        match nv with
                        | None          -> ()
                        | Some child    -> child.SetParent e    // Invalidates parent

            static let Persistent id valueChanged value = Property.Persistent<DecoratorElement, _>  id valueChanged value 

            let mutable cachedChildren : Element array option = None


            static member Child     = Persistent     "Child"        InvalidateChild <| Value (None : Element option)


            override x.OnChildren () = 
                        let child = x.Get DecoratorElement.Child
                        match cachedChildren, child with
                        | Some c    , _         -> c
                        | None      , Some c    -> 
                            let children = [|c|]
                            cachedChildren <- Some children
                            children
                        | None      , None      ->
                            let children = [||]
                            cachedChildren <- Some children
                            children
                                    
            override x.OnMeasureContent a   =   
                        let child = x.Get DecoratorElement.Child
                        match child with
                        | None      -> Measurement.Zero
                        | Some c    -> c.MeasureElement a

            override x.OnPlaceElement o i   =   
                        let child = x.Get DecoratorElement.Child
                        match child with
                        | None      -> ()
                        | Some c    -> c.PlaceElement i

        type [<AbstractClass>] LayoutElement() = 
            inherit ContainerElement()
    
            let children    = SortedDictionary<int, Element>()

            let mutable cachedChildren = None

            override x.OnChildren ()  = match cachedChildren with
                                        | Some c    ->  c
                                        | None      ->  let c = children |> Seq.map (fun kv -> kv.Value) |> Seq.toArray
                                                        cachedChildren <- Some c
                                                        c


            member x.SetChild i le    = children.[i] <- le
                                        le.SetParent x  // Invalidates parent
                                        cachedChildren <- None
                                        x

            member x.RemoveChild i    = let c = children.Find i
                                        match c with
                                        | None      -> ()
                                        | Some le    -> 
                                            ignore <| children.Remove i
                                            le.ClearParent ()   // Invalidates old parent
                                            cachedChildren <- None
                                        x  

        type DocumentElement(ctx : ElementContext) as x=
            inherit DecoratorElement()

            do
                x.SetContext ctx
                
                

        type StackElement() = 
            inherit LayoutElement()

            let AccumulateMeasurement   (orientation : StackOrientation) (measurement : Measurement) (other : Measurement) = 
                match orientation with
                | FromLeft | FromRight  -> Measurement.New (measurement.Width + other.Width) measurement.Height
                | FromTop  | FromBottom -> Measurement.New measurement.Width (measurement.Height + other.Height) 
                
            let SubtractAvailable       (orientation : StackOrientation) (available : Available) (other : Measurement) = 
                match orientation with
                | FromLeft | FromRight  -> Available.New (available.Width - other.Width) available.Height
                | FromTop  | FromBottom -> Available.New available.Width (available.Height - other.Height) 

            let Intersect (f : float32) (m : MeasurementUnit) =
                match m with
                | FixedMeasurement m    -> Clamp <| min f m 
                | Fill                  -> f

            let Subtract (f : float32) (m : MeasurementUnit) =
                match m with
                | FixedMeasurement m    -> Clamp <| f - m 
                | Fill                  -> 0.F

            let AccumulateAndIntersectPlacement   (orientation : StackOrientation) (placement : Placement) (other : Measurement) = 
                match orientation with
                | FromLeft  -> let intersected  = Placement.New placement.X placement.Y (Intersect placement.Width other.Width) placement.Height
                               let adjusted     = Placement.New (placement.X + intersected.Width) placement.Y (Clamp <| placement.Width - intersected.Width) placement.Height
                               adjusted,intersected
                | FromRight -> let adjusted     = Placement.New placement.X placement.Y (Subtract placement.Width other.Width) placement.Height
                               let intersected  = Placement.New (placement.X + adjusted.Width) placement.Y (Clamp <| placement.Width - adjusted.Width) placement.Height
                               adjusted,intersected
                | FromTop   -> let intersected  = Placement.New placement.X placement.Y placement.Width (Intersect placement.Height other.Height)
                               let adjusted     = Placement.New placement.X (placement.Y + intersected.Height) placement.Width (Clamp <| placement.Height - intersected.Height)
                               adjusted,intersected
                | FromBottom-> let adjusted     = Placement.New placement.X placement.Y placement.Width (Subtract placement.Height other.Height) 
                               let intersected  = Placement.New placement.X (placement.Y + adjusted.Height) placement.Width (Clamp <| placement.Height - adjusted.Height) 
                               adjusted,intersected
                
            static let Persistent id valueChanged value = Property.Persistent<StackElement, _>  id valueChanged value 

            static member Orientation   = Persistent "Orientation"     InvalidateMeasurement    <| Value StackOrientation.FromTop

            override x.OnMeasureContent a   = 
                        let orientation = x.Get StackElement.Orientation

                        let mutable measurement = Measurement.Zero
                        let mutable remaining   = a

                        let children = x.Children
                        for c in children do
                            let cm = c.MeasureElement a

                            measurement <- AccumulateMeasurement orientation measurement cm
                            remaining   <- SubtractAvailable orientation remaining cm
    
                        measurement
    
            override x.OnPlaceElement o i   = 
                        let orientation = x.Get StackElement.Orientation

                        let mutable placement = i

                        let children = x.Children
                        for c in children do
                            let cachedMeasurement = c.Get Element.Measurement
                            match cachedMeasurement with
                            | None      -> ()
                            | Some m    ->
                                let a,i = AccumulateAndIntersectPlacement orientation placement m
                                c.PlaceElement i
                                placement <- a
    
                        ()

        [<AbstractClass>]
        type TextElement() =
            inherit Element()

            static let Persistent id valueChanged value = Property.Persistent<TextElement, _> id valueChanged value 

            static member Text          = Persistent     "Text"        InvalidateMeasurement  <| Value ""              

        type LabelElement() =
            inherit TextElement()

            override x.OnMeasureContent a = 
                        let context = x.Context
                        match context with
                        | None          -> Debug.Assert false; Measurement.Fill
                        | Some context  -> 
                            let text = x.Get TextElement.Text
                            if text.Length = 0 then Measurement.Zero
                            else
                                let tfd = x.Get Element.TextFormatDescriptor
                                let size = context.MeasureText tfd (a.ToSize2F ()) text
                                Measurement.FromSize2 size


            override x.OnRenderContent (o : Placement)
                                       (i : Placement) =
                            let text = x.Get TextElement.Text
                            if text = "" then VisualTree.NoVisual
                            else 
                                let foreground = x.Get Element.Foreground
                                let fontFamily = x.Get Element.FontFamily
                                let fontSize = x.Get Element.FontSize
                                let textFormatDescriptor = TextFormatDescriptor.New fontFamily fontSize
                                let layoutRect = i.ToRectangleF () |> Animated.Constant
                                VisualTree.Text (text, textFormatDescriptor, layoutRect, foreground |> Animated.Brush.Solid)
    
        type ButtonState =
            | Normal
            | Highlighted
            | Pressed

        type ButtonElement() =
            inherit DecoratorElement()

            static let Persistent   id valueChanged value   = Property.Persistent<ButtonElement, _> id valueChanged value 
            static let Routed       id sample               = Event.Routed<ButtonElement, _> id sample

            static do
                Element.Foreground.Override<ButtonElement> (Some <| Value (SolidBrush Color.White)) None
                Element.Background.Override<ButtonElement> (Some <| Value (SolidBrush Color.Black)) None

            static member ButtonState       = Persistent    "ButtonState"      InvalidateVisual        <| Value ButtonState.Normal

            static member Highlight         = Persistent    "Highlight"        InvalidateVisual        <| Value (SolidBrush Color.Purple      )
            static member Pressed           = Persistent    "Pressed"          InvalidateVisual        <| Value (SolidBrush Color.LightBlue   )
            static member Border            = Persistent    "Border"           InvalidateVisual        <| Value (SolidBrush Color.White       )
            static member BorderThickness   = Persistent    "BorderThickness"  InvalidateMeasurement   <| Value 2.0F           

            static member Clicked           = Routed        "Clicked"          ()

            override x.OnGetEffectiveMargin ()  = x.Get Element.Margin + (Thickness.Uniform <| x.Get ButtonElement.BorderThickness) + x.Get ContainerElement.Padding

            override x.OnRenderContent (o : Placement)
                                       (i : Placement) =
                            let r = (o + x.Get Element.Margin).ToRectangleF ()

                            let state = x.Get ButtonElement.ButtonState
                            let borderThickness = x.Get ButtonElement.BorderThickness
                            let border          = x.Get ButtonElement.Border

                            let background = 
                                match state with
                                | Normal        -> x.Get ButtonElement.Background
                                | Highlighted   -> x.Get ButtonElement.Highlight
                                | Pressed       -> x.Get ButtonElement.Pressed
                            VisualTree.Rectangle (
                                border |> Animated.Brush.Solid      , 
                                background |> Animated.Brush.Solid  , 
                                r |> Animated.Constant              , 
                                borderThickness |> Animated.Constant
                                )
    
    module Properties = 

        open Foundation
        open Standard

        let Bounds          = Element.Bounds              
        let IsVisible       = Element.IsVisible           
        let Margin          = Element.Margin              
        let FontFamily      = Element.FontFamily          
        let FontSize        = Element.FontSize            
        let Background      = Element.Background          
        let Foreground      = Element.Foreground          
    
        let Padding         = ContainerElement.Padding

        let Child           = DecoratorElement.Child

        let Orientation     = StackElement.Orientation

        let Text            = TextElement.Text

    module Events = 

        open Foundation
        open Standard

        let Clicked         = ButtonElement.Clicked

    open Foundation
    open Standard

    let SomeElement (e : #Element) = Some (e :> Element)

    let ( >>+ ) (e : #Element) (el : EventListener<'T>) = 
                    ignore <| el.SetListener e
                    e
    let ( >>- ) (e : #Element) (el : EventListener<'T>) = 
                    ignore <| el.ClearListener e
                    e

    let CreateElement<'T when 'T :> Element and 'T : (new: unit -> 'T)> (pvs : PropertyValue list) = 
        let element = new 'T()
        element.AssignFromPropertyValues pvs
        element

    let CreateContainer<'T when 'T :> LayoutElement and 'T : (new: unit -> 'T)> (pvs : PropertyValue list) (children : Element list) = 
        let layout = CreateElement<'T> pvs
        let mutable i = 0
        for child in children do
            ignore <| layout.SetChild i child
            i <- i + 1
        layout

    let Label (pvs : PropertyValue list) : LabelElement  = CreateElement<LabelElement> pvs

    let Stack (pvs : PropertyValue list) (children : Element list)  = CreateContainer<StackElement> pvs children

    let Button (pvs : PropertyValue list) = CreateElement<ButtonElement> pvs

    let TextButton text (pvs : PropertyValue list) = 
        let pv : PropertyValue = upcast Properties.Child.Value (SomeElement <| Label [ Properties.Text.Value text])
        Button <| pv::pvs