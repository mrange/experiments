namespace RayTracer

open System.Globalization
open System.Text

type Vector2 = 
    {
        X   : float
        Y   : float
    }


    static member New x y = {X = x; Y = y}
    static member Zero = Vector2.New 0. 0.
    static member One = Vector2.New 1. 1.
    static member (+) (x : Vector2, y : Vector2) = Vector2.New (x.X + y.X) (x.Y + y.Y)
    static member (-) (x : Vector2, y : Vector2) = Vector2.New (x.X - y.X) (x.Y - y.Y)
    static member (*) (x : Vector2, y : Vector2) = x.X * y.X + x.Y * y.Y

    member x.Scale s    = Vector2.New (s * x.X) (s * x.Y)
    member x.Normalize  = x.Scale (1. / x.L1)
    member x.L2         = x * x
    member x.L1         = sqrt x.L2
    member x.Min y      = Vector2.New (min x.X y.X) (min x.Y y.Y)
    member x.Max y      = Vector2.New (max x.X y.X) (max x.Y y.Y)

    override x.ToString () = 
        let sb = new StringBuilder()
        sb
            .Append('(')
            .Append(x.X.ToString(CultureInfo.InvariantCulture))
            .Append(',')
            .Append(x.Y.ToString(CultureInfo.InvariantCulture))
            .Append(')')
            .ToString()

type Vector3 = 
    {
        X   : float
        Y   : float
        Z   : float
    }


    static member New x y z = {X = x; Y = y; Z = z}
    static member Zero = Vector3.New 0. 0. 0.
    static member One = Vector3.New 1. 1. 1.
    static member (+) (x : Vector3, y : Vector3) = Vector3.New (x.X + y.X) (x.Y + y.Y) (x.Z + y.Z)
    static member (-) (x : Vector3, y : Vector3) = Vector3.New (x.X - y.X) (x.Y - y.Y) (x.Z - y.Z)
    static member (*) (x : Vector3, y : Vector3) = x.X * y.X + x.Y * y.Y + x.Z * y.Z
    static member ( *+* ) (x : Vector3, y : Vector3) = Vector3.New (x.Y * y.Z - x.Z * y.Y) (x.Z * y.X - x.X * y.Z) (x.X * y.Y - x.Y * y.X)

    member x.Scale s    = Vector3.New (s * x.X) (s * x.Y) (s * x.Z)
    member x.Normalize  = x.Scale (1. / x.L1)
    member x.L2         = x * x
    member x.L1         = sqrt x.L2
    member x.Min y      = Vector3.New (min x.X y.X) (min x.Y y.Y) (min x.Z y.Z)
    member x.Max y      = Vector3.New (max x.X y.X) (max x.Y y.Y) (max x.Z y.Z)

    member x.ComputeNormal () = 
        if x.X <> 0. then Vector3.New (-(x.Y + x.Z) / x.X) 1. 1.
        elif x.Y <> 0. then Vector3.New 1. (-(x.X + x.Z) / x.Y) 1.
        elif x.Z <> 0. then Vector3.New 1. 1. (-(x.Y + x.X) / x.Z) 
        else Vector3.One
        

    override x.ToString () = 
        let sb = new StringBuilder()
        sb
            .Append('(')
            .Append(x.X.ToString(CultureInfo.InvariantCulture))
            .Append(',')
            .Append(x.Y.ToString(CultureInfo.InvariantCulture))
            .Append(',')
            .Append(x.Z.ToString(CultureInfo.InvariantCulture))
            .Append(')')
            .ToString()
