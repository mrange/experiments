namespace FolderSize

module Units = 

    type PositionUnit = 
        | AutoPos
        | MinPos
        | MaxPos
        | FixedPos  of float32
            

    type SizeUnit = 
        | MinSize
        | MaxSize
        | FixedSize of float32
        member x.Natural with get () =  match x with
                                        | FixedSize fs  -> FixedSize <| Natural fs
                                        | _             -> x

    type Bounds = 
        {
            X       : PositionUnit
            Y       : PositionUnit
            Width   : SizeUnit
            Height  : SizeUnit
        }
        static member New x y (w : SizeUnit) (h : SizeUnit) = {X = x; Y = y; Width = w.Natural; Height = h.Natural}
        static member Min = Bounds.New MinPos MinPos MinSize MinSize

    type ThickessUnit = float32
    type Thickness =
        {
            Left    : ThickessUnit
            Top     : ThickessUnit
            Right   : ThickessUnit
            Bottom  : ThickessUnit
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

    type MeasurementUnit = 
        |   FixedMeasurement    of float32
        static member Zero = FixedMeasurement 0.F

        static member ( + ) (l : MeasurementUnit, r : float32) = 
            match l with
            | FixedMeasurement  v -> FixedMeasurement (Natural <| v + r)
        static member ( - ) (l : MeasurementUnit, r : float32) = l + (-r)

        member x.Union (o : MeasurementUnit) = 
            match x,o with
            | FixedMeasurement xx   , FixedMeasurement yy   -> FixedMeasurement <| max xx yy

        member x.Natural with get () =  match x with
                                        | FixedMeasurement b    -> FixedMeasurement <| Natural b


    type Measurement = 
        {
            Width   : MeasurementUnit
            Height  : MeasurementUnit
        }
        static member New (w : MeasurementUnit) (h : MeasurementUnit) = {Width = w.Natural; Height = h.Natural}
        static member Zero = Measurement.New MeasurementUnit.Zero MeasurementUnit.Zero

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
                                        | _             -> x

        member x.IsMeasurementValid (m : MeasurementUnit) = 
                match x,m with
                | Unbound   , _                 -> true
                | Bound b   , FixedMeasurement v-> b >= v


    type AvailableArea = 
        {
            Width   : AvailableUnit
            Height  : AvailableUnit
        }
        static member New (w : AvailableUnit) (h : AvailableUnit) = {Width = w.Natural; Height = h.Natural}
        static member Unbound = AvailableArea.New AvailableUnit.Unbound AvailableUnit.Unbound
        static member Zero = AvailableArea.New AvailableUnit.Zero AvailableUnit.Zero

        static member ( + ) (l : AvailableArea, r : Thickness) = 
                            AvailableArea.New
                                (l.Width    + (r.Left + r.Right ))
                                (l.Height   + (r.Top  + r.Bottom))

        static member ( - ) (l : AvailableArea, r : Thickness) = l + (-r)     

        member x.IsMeasurementValid (m : Measurement) = x.Width.IsMeasurementValid m.Width && x.Height.IsMeasurementValid m.Height


    type PlacementUnit = float32

    type Placement =
        {
            X       : PlacementUnit
            Y       : PlacementUnit
            Width   : PlacementUnit
            Height  : PlacementUnit
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

