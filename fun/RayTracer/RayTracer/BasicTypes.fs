namespace RayTracer

type Color = 
    {
        Red     : float
        Green   : float
        Blue    : float
    }
    static member New red green blue = {Red = unorm red; Green = unorm green; Blue = unorm blue}
    static member Zero = Color.New 0. 0. 0.
    static member (+) (x : Color, y : Color) = Color.New (x.Red + y.Red) (x.Green + y.Green) (x.Blue + y.Blue)
    static member (*) (x : Color, y : Color) = Color.New (x.Red * y.Red) (x.Green * y.Green) (x.Blue * y.Blue)
    member x.Dim t = Color.New (t * x.Red) (t * x.Green) (t * x.Blue)


type Material = 
    {
        Color       : Color
        Opacity     : float
        Diffusion   : float
        Refraction  : float
        Reflection  : float
    }
    static member New color opacity diffusion refraction reflection = {Color = color; Opacity = unorm opacity; Diffusion = unorm diffusion; Refraction = unorm refraction; Reflection = unorm reflection}


type Surface = Vector2 -> Material

type Ray = 
    {
        Direction   : Vector3
        Origin      : Vector3
    }
    member x.Trace t            = (x.Direction.Scale t) + x.Origin
    member x.IntersectSphere (center :Vector3) (radius : float) = 
        let v = x.Origin - center
        let vd = v * x.Direction
        let v2 = v.L2
        let r2 = radius * radius

        let discriminant = vd*vd - v2 + r2
        if discriminant < 0. then None
        else
            let root = sqrt discriminant
            let t1 = -vd + root
            let t2 = -vd - root


            if t1 < cutoff || t2 < cutoff then None
            elif t1 < t2 then Some (t1, t2)
            else Some (t2, t1)
    member x.IntersectPlane (normal :Vector3) (offset : float) = 
        let t = -(x.Origin*normal + offset) / (x.Direction*normal)
        if t > cutoff then Some t
        else None
        
    static member FromTo (origin : Vector3) (destination: Vector3) = {Direction = (destination - origin).Normalize; Origin = origin}
    static member DirectionOrigin (direction: Vector3) (origin : Vector3) = {Direction = direction.Normalize; Origin = origin}

