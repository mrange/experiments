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

    type Position = 
        |   AutoPos
        |   MinPos
        |   MaxPos
        |   FixedPos    of float32

    type Size = 
        |   AutoSize
        |   FillSize
        |   FixedSize   of float32


    type Thickness =
        {
            Left    : float32
            Top     : float32
            Right   : float32
            Bottom  : float32
        }
        static member New l t r b = {Left = l; Top = t; Right = r; Bottom = b}
        static member Empty = Thickness.New 0.F 0.F 0.F 0.F

        static member ( + ) (l : Thickness, r : Thickness) = 
                            Thickness.New 
                                (l.Left      + r.Left   )
                                (l.Top       + r.Top    )
                                (l.Right     + r.Right  )
                                (l.Bottom    + r.Bottom )

    type Rect = 
        {
            X       : Position
            Y       : Position
            Width   : Size
            Height  : Size
        }
        static member New x y w h = {X = x; Y = y; Width = w; Height = h}
        static member Empty = Rect.New MinPos MinPos AutoSize AutoSize

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

        static member InvalidateMeasurement   (le : Element) (ov : 'T) (nv : 'T) =  le.InvalidateMeasurement   ()
        static member InvalidatePlacement     (le : Element) (ov : 'T) (nv : 'T) =  le.InvalidatePlacement     ()
        static member InvalidateVisual        (le : Element) (ov : 'T) (nv : 'T) =  le.InvalidateVisual        ()


    and Property<'T>(id : string, valueCreator : unit -> 'T, valueChanged : Element -> 'T -> 'T -> unit)= 
        inherit Property(id, typeof<'T>)    

        override x.OnCreateValue ()                     = 
                    upcast valueCreator () 
        override x.OnValueChanged le oldValue newValue  = 
                    let ov = oldValue.CastTo Unchecked.defaultof<'T>
                    let nv = newValue.CastTo Unchecked.defaultof<'T>
                    valueChanged le ov nv

        static member ( * ) (p : Property<'T>, (v : 'T)) = PropertyValue(p, v)

    and PropertyValue(p : Property, v : obj)= 
        
        member x.Property   = p
        member x.Value      = v

    and Element() = 

        let mutable parent : Container option = None

        let properties = Dictionary<Property, obj>()

        member x.Parent 
            with get ()   = parent
            and set p = parent <- p

        member x.GetPropertyValue   (lp :Property<'T>)           : 'T = 
                Unchecked.defaultof<'T>
        member x.SetPropertyValue   (lp :Property<'T>) (v : 'T)  : unit = 
                ()
        member x.ClearPropertyValue (lp :Property<'T>)           : unit = 
                ()

        member x.InvalidateMeasurement  () = ()            
        member x.InvalidatePlacement    () = ()            
        member x.InvalidateVisual       () = ()            

        member x.RenderElement  () = NoVisual
        member x.RenderOverlay  () = NoVisual

        member x.MeasureElement (sz : Size2F)       = let t = (x --> Element.Margin) + (x --> Element.Border) + (x --> Element.Padding)
                                                      Size2F()
        member x.PlaceElement   (r  : RectangleF)   = r

        static member ( --> ) (e : Element, p : Property<'T>) = e.GetPropertyValue p

        static member Bounds            = Property.Create "Bounds"          Property.InvalidateMeasurement  (Property.Value Rect.Empty      )
        static member Margin            = Property.Create "Margin"          Property.InvalidateMeasurement  (Property.Value Thickness.Empty )
        static member Border            = Property.Create "Border"          Property.InvalidateMeasurement  (Property.Value Thickness.Empty )
        static member Padding           = Property.Create "Padding"         Property.InvalidateMeasurement  (Property.Value Thickness.Empty )
        static member FontFamily        = Property.Create "FontFamily"      Property.InvalidateMeasurement  (Property.Value "Verdana"       )
        static member FontSize          = Property.Create "FontSize"        Property.InvalidateMeasurement  (Property.Value 12.F            )

        static member BorderBrush       = Property.Create "BorderBrush"     Property.InvalidateMeasurement  (Property.Value <| BrushDescriptor.Transparent)
        static member BackgroundBrush   = Property.Create "BackgroundBrush" Property.InvalidateMeasurement  (Property.Value <| BrushDescriptor.Transparent)

    and Container() = 
        inherit Element()
    
        let children    = SortedDictionary<int, Element>()

        let mutable cachedChildren = None

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

    type Text() =
        inherit Element()

        static member Text          = Property.Create "Text"        Property.InvalidateMeasurement  (Property.Value ""              )

    type Div() = 
        inherit Container ()

    module Elements = 
        let Text (ps : PropertyValue list) : Text  = Text()

        let Div (ps : PropertyValue list) (children : Element list)  = ()

        let body = 
            Div [
                    Element.FontFamily * ""
                ]
                [
                    Text []
                    Text []
                    Text []
                ]

