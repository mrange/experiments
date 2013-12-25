namespace FolderSize

open System
open System.Collections.Generic
open System.Runtime.InteropServices

open SharpDX

module Logical = 

    type LayoutRotation = 
        | D0
        | D90
        | D180
        | D270

    type MeasuredSize = 
        | Unbound
        | Bound     of float32 

        static member Zero = Bound 0.F

        static member ( + ) (l : MeasuredSize, r : float32) = 
            match l with
            | Unbound   -> Unbound
            | Bound b   -> Bound (Natural <| b + r)
        static member ( - ) (l : MeasuredSize, r : float32) = l + (-r)

        member x.Max (o : MeasuredSize) = 
            match x,o with
            | Bound xx  , Bound yy  -> Bound <| max xx yy
            | _         , _         -> Unbound

        member x.Natural with get () =  match x with
                                        | Bound b       -> Bound <| Natural b
                                        | _             -> x

    type Position = 
        | AutoPos
        | MinPos
        | MaxPos
        | FixedPos  of float32
            

    type Size = 
        | MinSize
        | MaxSize
        | FixedSize of float32
        member x.Natural with get () =  match x with
                                        | FixedSize fs  -> FixedSize <| Natural fs
                                        | _             -> x


    type Thickness =
        {
            Left    : float32
            Top     : float32
            Right   : float32
            Bottom  : float32
        }
        static member New l t r b   = {Left = Natural l; Top = Natural t; Right = Natural r; Bottom = Natural b}
        static member Zero          = Thickness.New 0.F 0.F 0.F 0.F

        member x.IsZero with get () = x = Thickness.Zero

        static member ( + ) (l : Thickness, r : Thickness) = 
                            Thickness.New 
                                (l.Left      + r.Left   )
                                (l.Top       + r.Top    )
                                (l.Right     + r.Right  )
                                (l.Bottom    + r.Bottom )

        static member ( ~- ) (t : Thickness) = 
                            Thickness.New 
                                -t.Left      
                                -t.Top       
                                -t.Right     
                                -t.Bottom    

    type Measurement = 
        {
            Width   : MeasuredSize
            Height  : MeasuredSize
        }
        static member New (w : MeasuredSize) (h : MeasuredSize) = {Width = w.Natural; Height = h.Natural}
        static member Unbound = Measurement.New Unbound Unbound
        static member Zero = Measurement.New MeasuredSize.Zero MeasuredSize.Zero

        static member ( + ) (l : Measurement, r : Thickness) = 
                            Measurement.New
                                (l.Width    + (r.Left + r.Right ))
                                (l.Height   + (r.Top  + r.Bottom))

        static member ( - ) (l : Measurement, r : Thickness) = 
                            Measurement.New
                                (l.Width    - (r.Left + r.Right ))
                                (l.Height   - (r.Top  + r.Bottom))

        member x.Max (o : Measurement) = 
            Measurement.New 
                (x.Width.Max    o.Width )
                (x.Height.Max   o.Height)

    type Placement =
        {
            X       : float32
            Y       : float32
            Width   : float32
            Height  : float32
        }
        static member New x y w h = {X = x; Y = y; Width = Natural w; Height = Natural h}
        static member Zero = Placement.New 0.F 0.F 0.F 0.F

        member x.IsZero with get ()     = x = Placement.Zero
        member x.IsEmpty with get ()    = x.Width <= 0.F && x.Height <= 0.F



        static member ( + ) (l : Placement, r : Thickness) = 
                            Placement.New
                                (l.X - r.Left                               )
                                (l.Y - r.Top                                )
                                (Natural <| l.Width     + r.Left+ r.Right   )
                                (Natural <| l.Height    + r.Top + r.Bottom  )

        static member ( - ) (l : Placement, r : Thickness) = 
                            Placement.New
                                (l.X + r.Left                               )
                                (l.Y + r.Top                                )
                                (Natural <| l.Width     - r.Left- r.Right   )
                                (Natural <| l.Height    - r.Top - r.Bottom  )

    type Rect = 
        {
            X       : Position
            Y       : Position
            Width   : Size
            Height  : Size
        }
        static member New x y (w : Size) (h : Size) = {X = x; Y = y; Width = w.Natural; Height = h.Natural}
        static member MinMin = Rect.New MinPos MinPos MinSize MinSize

    type LayoutTransform = 
        {
            Rotation : LayoutRotation
            Scaling  : float32
        }


    [<AbstractClass>]
    type Property(id : string, ``type`` : Type)= 

        member x.Id             = id
        member x.Type           = ``type``

        abstract OnCreateValue  : unit -> obj
        abstract OnValueChanged : Element -> obj -> obj -> unit

        member x.CreateValue                            = x.OnCreateValue ()
        member x.ValueChanged le oldValue newValue      = x.OnValueChanged le oldValue newValue

        static member Value v = fun () -> v
        static member Create id valueChanged valueCreator = Property<'T>(id,valueCreator,valueChanged)

    and Property<'T>(id : string, valueCreator : unit -> 'T, valueChanged : Element -> 'T -> 'T -> unit)= 
        inherit Property(id, typeof<'T>)    

        override x.OnCreateValue ()                     = 
                    upcast valueCreator () 
        override x.OnValueChanged le oldValue newValue  = 
                    let ov = oldValue.CastTo Unchecked.defaultof<'T>
                    let nv = newValue.CastTo Unchecked.defaultof<'T>
                    valueChanged le ov nv

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
        
        let mutable parent : Container option = None

        static let __NoAction              (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()
        static let __InvalidateMeasurement (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateMeasurement   ()
        static let __InvalidatePlacement   (le : Element) (ov : 'T) (nv : 'T) = le.InvalidatePlacement     ()
        static let __InvalidateVisual      (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateVisual        ()

        static let __Value  (v : 'T)    = Property.Value v

        let properties = Dictionary<Property, obj>()

        static member Measurement       = Property.Create "Measurement"     __NoAction              (__Value (None : Measurement option))
        static member Placement         = Property.Create "Placement"       __NoAction              (__Value (None : Placement option))
                                                                                                     
        static member Bounds            = Property.Create "Bounds"          __InvalidateMeasurement (__Value Rect.MinMin    )
        static member IsVisible         = Property.Create "IsVisible"       __InvalidateMeasurement (__Value true           )
                                                                            
        static member Margin            = Property.Create "Margin"          __InvalidateMeasurement (__Value Thickness.Zero )
                                                                            
        static member FontFamily        = Property.Create "FontFamily"      __InvalidateMeasurement (__Value "Verdana"      )
        static member FontSize          = Property.Create "FontSize"        __InvalidateMeasurement (__Value 12.F           )
                                                                            
                                                                            
        static member BackgroundBrush   = Property.Create "BackgroundBrush" __InvalidateMeasurement (__Value <| BrushDescriptor.Transparent)

        member x.Parent 
            with get ()         = parent
            and private set p   = parent <- p

        member x.Get    (lp :Property<'T>)           : 'T = 
                Unchecked.defaultof<'T>
        member x.Set    (lp :Property<'T>) (v : 'T)  : unit = 
                ()
        member x.Clear  (lp :Property<'T>)           : unit = 
                ()

        member x.NoAction               () = ()            
        member x.InvalidateMeasurement  () = ()            
        member x.InvalidatePlacement    () = ()            
        member x.InvalidateVisual       () = ()            

        abstract OnGetBox                           : unit -> Thickness
        default x.OnGetBox ()                       = x.Get Element.Margin

        member x.Box                                = x.OnGetBox ()

        abstract OnMeasureContent                   : Measurement -> Measurement
        default x.OnMeasureContent m                = m
        member x.MeasureElement (m : Measurement)   = let box = x.Box
                                                      let innerMeasure = x.OnMeasureContent<| m - box
                                                      innerMeasure + box

        abstract OnPlaceContent                     : Placement -> unit
        default x.OnPlaceContent p                  = ()
        member x.PlaceElement   (p : Placement)     = let box = x.Box
                                                      x.OnPlaceContent  <| p - box

        abstract OnRenderContent                    : Placement -> Placement -> VisualTree
        default x.OnRenderContent   (o : Placement)
                                    (i : Placement)
                                                    = VisualTree.NoVisual
        member x.RenderContent  ()                  = let box = x.Box
                                                      let p = x.Get Element.Placement
                                                      match p with
                                                      | Some pp -> x.OnRenderOverlay pp <| pp - box
                                                      | _       -> NoVisual

        abstract OnRenderOverlay                    : Placement -> Placement -> VisualTree
        default x.OnRenderOverlay   (o : Placement)
                                    (i : Placement)
                                                    = VisualTree.NoVisual
        member x.RenderOverlay  ()                  = let box = x.Box
                                                      let p = x.Get Element.Placement
                                                      match p with
                                                      | Some pp -> x.OnRenderOverlay pp <| pp - box
                                                      | _       -> NoVisual
                                                        

    and Container() = 
        inherit Element()
    
        let children    = SortedDictionary<int, Element>()

        let mutable cachedChildren = None

        static let __NoAction              (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()
        static let __InvalidateMeasurement (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateMeasurement   ()
        static let __InvalidatePlacement   (le : Element) (ov : 'T) (nv : 'T) = le.InvalidatePlacement     ()
        static let __InvalidateVisual      (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateVisual        ()

        static let __Value  (v : 'T)    = Property.Value v

        static member Padding           = Property.Create "Padding"         __InvalidateMeasurement (__Value Thickness.Zero )

        member x.Children with get () = match cachedChildren with
                                        | Some c    ->  c
                                        | _         ->  let c = children |> Seq.map (fun kv -> kv.Value) |> Seq.toList
                                                        cachedChildren <- Some c
                                                        c


        member x.InsertChild i le = ignore <| (children.[i] = le)
                                    cachedChildren <- None
                                    x

        member x.RemoveChild i =    ignore <| children.Remove i
                                    cachedChildren <- None
                                    x  

        override x.OnGetBox ()          = x.Get Element.Margin + x.Get Container.Padding

        override x.OnMeasureContent m   = let children = x.Children
                                          let sz : Measurement option = None
                                          for c in children do
                                            // TODO:


    let NoAction                (le : Element) (ov : 'T) (nv : 'T) = le.NoAction                ()
    let InvalidateMeasurement   (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateMeasurement   ()
    let InvalidatePlacement     (le : Element) (ov : 'T) (nv : 'T) = le.InvalidatePlacement     ()
    let InvalidateVisual        (le : Element) (ov : 'T) (nv : 'T) = le.InvalidateVisual        ()

    let Value   (v : 'T)    = Property.Value v


    type Text() =
        inherit Element()

        static member Text          = Property.Create "Text"        InvalidateMeasurement  (Property.Value ""              )

    type Div() = 
        inherit Container ()

    module Elements = 
        let Text (ps : PropertyValue list) : Text  = Text()

        let Div (ps : PropertyValue list) (children : Element list)  = ()

        let body = 
            Div [
                    Element.Bounds.Value        Rect.MinMin
                    Element.FontFamily.Value    ""
                ]
                [
                    Text []
                    Text []
                    Text []
                ]

