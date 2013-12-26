namespace FolderSize

module Units = 

    [<StructuralEquality>]
    [<StructuralComparison>]
    type PositionUnit = 
        | AutoPos
        | MinPos
        | MaxPos
        | FixedPos  of float32
            

    [<StructuralEquality>]
    [<StructuralComparison>]
    type SizeUnit = 
        | MinSize
        | MaxSize
        | FixedSize of float32
        member x.Natural with get () =  match x with
                                        | FixedSize fs  -> FixedSize <| Natural fs
                                        | _             -> x

    [<StructuralEquality>]
    [<StructuralComparison>]
    type Bounds = 
        {
            X       : PositionUnit
            Y       : PositionUnit
            Width   : SizeUnit
            Height  : SizeUnit
        }
        static member New x y (width : SizeUnit) (height : SizeUnit) = {X = x; Y = y; Width = width.Natural; Height = height.Natural}
        static member Min = Bounds.New MinPos MinPos MinSize MinSize

    type ThickessUnit = float32

    [<StructuralEquality>]
    [<StructuralComparison>]
    type Thickness =
        {
            Left    : ThickessUnit
            Top     : ThickessUnit
            Right   : ThickessUnit
            Bottom  : ThickessUnit
        }
        static member New left top right bottom     = {Left = Natural left; Top = Natural top; Right = Natural right; Bottom = Natural bottom}
        static member Zero                          = Thickness.New 0.F 0.F 0.F 0.F

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

    [<StructuralEquality>]
    [<StructuralComparison>]
    type MeasurementUnit = 
        | FixedMeasurement  of float32
        | Fill
        static member Zero = FixedMeasurement 0.F

        static member ( + ) (l : MeasurementUnit, r : float32) = 
            match l with
            | FixedMeasurement  v   -> FixedMeasurement (Natural <| v + r)
            | Fill                  -> Fill
        static member ( - ) (l : MeasurementUnit, r : float32) = l + (-r)

        member x.Union (o : MeasurementUnit) = 
            match x,o with
            | Fill              , _                     -> Fill
            | _                 , Fill                  -> Fill
            | FixedMeasurement l, FixedMeasurement r    -> FixedMeasurement <| max l r

        member x.Natural with get () =  match x with
                                        | FixedMeasurement b-> FixedMeasurement <| Natural b
                                        | Fill              -> Fill


    [<StructuralEquality>]
    [<StructuralComparison>]
    type Measurement = 
        {
            Width   : MeasurementUnit
            Height  : MeasurementUnit
        }
        static member New (width : MeasurementUnit) (height : MeasurementUnit) = {Width = width.Natural; Height = height.Natural}
        static member Zero = Measurement.New MeasurementUnit.Zero MeasurementUnit.Zero
        static member Fill = Measurement.New Fill Fill

        static member ( + ) (l : Measurement, r : Thickness) = 
                            Measurement.New
                                (l.Width    + (r.Left + r.Right ))
                                (l.Height   + (r.Top  + r.Bottom))

        static member ( - ) (l : Measurement, r : Thickness) = 
                            Measurement.New
                                (l.Width    - (r.Left + r.Right ))
                                (l.Height   - (r.Top  + r.Bottom))

        member x.Union (o : Measurement) = 
            Measurement.New 
                (x.Width.Union  o.Width )
                (x.Height.Union o.Height)


    [<StructuralEquality>]
    [<StructuralComparison>]
    type AvailableUnit = 
        | Unbound
        | Bound     of float32 

        static member Zero = Bound 0.F

        static member ( + ) (l : AvailableUnit, r : float32) = 
            match l with
            | Unbound   -> Unbound
            | Bound v   -> Bound (Natural <| v + r)
        static member ( - ) (l : AvailableUnit, r : float32) = l + (-r)

        member x.Max (o : AvailableUnit) = 
            match x,o with
            | Bound xx  , Bound yy  -> Bound <| max xx yy
            | _         , _         -> Unbound

        member x.Natural with get () =  match x with
                                        | Bound b       -> Bound <| Natural b
                                        | Unbound       -> x

        member x.IsMeasurementValid (measurement : MeasurementUnit) = 
                match x,measurement with
                | Unbound   , _                 -> true
                | Bound _   , Fill              -> true
                | Bound b   , FixedMeasurement v-> b >= v

        member x.ClipMeasurement (measurement : MeasurementUnit) = 
                match x,measurement with
                | Unbound   , _                             -> measurement
                | Bound _   , Fill                          -> measurement
                | Bound b   , FixedMeasurement v when b < v -> FixedMeasurement b
                | Bound b   , FixedMeasurement v            -> measurement


    [<StructuralEquality>]
    [<StructuralComparison>]
    type Available = 
        {
            Width   : AvailableUnit
            Height  : AvailableUnit
        }
        static member New (width : AvailableUnit) (height : AvailableUnit) = {Width = width.Natural; Height = height.Natural}
        static member Unbound = Available.New AvailableUnit.Unbound AvailableUnit.Unbound
        static member Zero = Available.New AvailableUnit.Zero AvailableUnit.Zero

        static member ( + ) (l : Available, r : Thickness) = 
                            Available.New
                                (l.Width    + (r.Left + r.Right ))
                                (l.Height   + (r.Top  + r.Bottom))

        static member ( - ) (l : Available, r : Thickness) = l + (-r)     

        member x.IsMeasurementValid (m : Measurement)   = x.Width.IsMeasurementValid m.Width && x.Height.IsMeasurementValid m.Height
        member x.ClipMeasurement (m : Measurement)      = Measurement.New (x.Width.ClipMeasurement m.Width) (x.Height.ClipMeasurement m.Height)


    type PlacementUnit = float32

    [<StructuralEquality>]
    [<StructuralComparison>]
    type Placement =
        {
            X       : PlacementUnit
            Y       : PlacementUnit
            Width   : PlacementUnit
            Height  : PlacementUnit
        }
        static member New x y width height = {X = x; Y = y; Width = Natural width; Height = Natural height}
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

