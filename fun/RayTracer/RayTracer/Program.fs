open System

[<AutoOpen>]
module Util =
    let sign d = if d < 0. then -1. else 1.
    let pi = Math.PI

    let clamp x min max = 
        if x < min then min
        elif x > max then max
        else x

    let norm x = clamp x -1. 1.
    let unorm x = clamp x 0. 1.
            

type Color = 
    {
        Red     : double
        Green   : double
        Blue    : double
    }
    static member New red green blue = {Red = unorm red; Green = unorm green; Blue = unorm blue}

type Material = 
    {
        Color       : Color
        Opacity     : double
        Diffusion   : double
        Refraction  : double
        Reflection  : double
    }
    static member New color opacity diffusion refraction reflection = {Color = color; Opacity = unorm opacity; Diffusion = unorm diffusion; Refraction = unorm refraction; Reflection = unorm reflection}

type Vector2 = 
    {
        X   : double
        Y   : double
    }


    static member New x y = {X = x; Y = y}
    static member Zero = Vector2.New 0. 0.
    static member (+) (x : Vector2, y : Vector2) = Vector2.New (x.X + y.X) (x.Y + y.Y)
    static member (-) (x : Vector2, y : Vector2) = Vector2.New (x.X - y.X) (x.Y - y.Y)
    static member (*) (x : Vector2, y : Vector2) = x.X * y.X + x.Y * y.Y

    member x.Scale s    = Vector2.New (s * x.X) (s * x.Y)
    member x.L2         = x * x
    member x.L1         = sqrt x.L2
    member x.Min y      = Vector2.New (min x.X y.X) (min x.Y y.Y)
    member x.Max y      = Vector2.New (max x.X y.X) (max x.Y y.Y)

type Vector3 = 
    {
        X   : double
        Y   : double
        Z   : double
    }


    static member New x y z = {X = x; Y = y; Z = z}
    static member Zero = Vector3.New 0. 0. 0.
    static member (+) (x : Vector3, y : Vector3) = Vector3.New (x.X + y.X) (x.Y + y.Y) (x.Z + y.Z)
    static member (-) (x : Vector3, y : Vector3) = Vector3.New (x.X - y.X) (x.Y - y.Y) (x.Z - y.Z)
    static member (*) (x : Vector3, y : Vector3) = x.X * y.X + x.Y * y.Y + x.Z * y.Z
    static member ( *** ) (x : Vector3, y : Vector3) = Vector3.New (x.Y * y.Z - x.Z * y.Y) (x.Z * y.Y - x.Y*y.Z) (x.X * y.Y - x.Y * y.X)

    member x.Scale s    = Vector3.New (s * x.X) (s * x.Y) (s * x.Z)
    member x.L2         = x * x
    member x.L1         = sqrt x.L2
    member x.Min y      = Vector3.New (min x.X y.X) (min x.Y y.Y) (min x.Z y.Z)
    member x.Max y      = Vector3.New (max x.X y.X) (max x.Y y.Y) (max x.Z y.Z)

type ViewPort = 
    {
        Corner          : Vector3
        OppositeCorner  : Vector3
    }
    static member New (c :Vector3) (oc : Vector3) = {Corner = c.Min oc; OppositeCorner = c.Max oc}


type Surface = Vector2 -> Material

type Intersection =
    {
        Normal      : Vector3
        Point       : Vector3
        Material    : Material
    }
    static member New normal point material = {Normal = normal; Point = point; Material = material}

type Light = 
    {
        Color   : Color
        Origin  : Vector3
    }
    static member New color origin = {Color = color; Origin = origin}

type Ray = 
    {
        Direction   : Vector3
        Origin      : Vector3
    }
    member x.Trace t = (x.Direction.Scale t) + x.Origin
    static member New direction origin = {Direction = direction; Origin = origin}


[<AbstractClass>]
type Shape (surface: Surface) = 
    member x.Surface with get () = surface
    member x.AsShape with get () = x
    abstract Intersect  : Ray -> Intersection option


type Sphere (surface: Surface, center : Vector3, radius : double) =
    inherit Shape (surface)

    member x.NormalAndMaterial p =
        let n' = (p - center) 
        let n = n'.Scale (1. / (n'.L1))
        let y' = Vector3.New n.X center.Y n.Z
        let y = (sign (n.Y - center.Y)) * acos ((y' * n) / (y'.L1 * n.L1)) / (2. * pi) + 0.5
        let x' = Vector3.New n.X center.Y center.Z
        let x = (sign (n.Z - center.Z)) * acos ((x' * y') / (x'.L1 * y'.L1)) / (2. * pi) + 0.5

        let m = base.Surface <| Vector2.New x y

        n,m

    override x.Intersect r = 
        let v = r.Origin - center
        let _2vd = 2. * (v * r.Direction)
        let v2 = v.L2
        let r2 = radius * radius

        let discriminant = _2vd + v2 - r2
        if discriminant < 0. then None
        else
            let root = sqrt discriminant
            let t1 = (_2vd + root) / 2.
            let t2 = (_2vd - root) / 2.

            if t1 <= 0. && t2 <= 0. then None
            elif t1 > 0. && t1 < t2 then 
                let p = r.Trace t1
                let n,m = x.NormalAndMaterial p
                Some <| Intersection.New n p m
            else 
                let p = r.Trace t2
                let n,m = x.NormalAndMaterial p
                Some <| Intersection.New n p m

type Plane (surface: Surface, offset : double, normal : Vector3)=
    inherit Shape (surface)

    let N = 
        let n = normal.L1
        if n <> 0. then normal.Scale (1. / n)
        else Vector3.Zero

    let X = 
        
        if N.X <> 0. then Vector3.New (offset / N.X) 0. 0.
        elif N.Y <> 0. then Vector3.New 0. (offset / N.Y) 0.
        elif N.Z <> 0. then Vector3.New 0. 0. (offset / N.Z)
        else Vector3.Zero

    let Y = 
        let n = N.L1
        let x = normal.L1
        if n <> 0. && x <> 0. then (N *** X).Scale (1. / x) // n is either 1 or 0
        else Vector3.Zero

    override x.Intersect r = 
        let t = -(r.Origin*N + offset) / (r.Direction*normal)

        if t <= 0. then None
        else
            let p = r.Trace t

            let c = Vector2.New (X * p) (Y * p)

            let m = base.Surface c

            Some <| Intersection.New N p m

let White   = Color.New 1. 1. 1.
let Red     = Color.New 1. 0. 0.
let Green   = Color.New 0. 1. 0.
let Blue    = Color.New 0. 0. 1.
let Black   = Color.New 0. 0. 0.
let Matte c = Material.New c 1. 1. 0. 0.

let UniformSurface (material : Material) : Surface = fun v -> material

[<EntryPoint>]
let main argv = 

    let lights = 
       [
            Light.New White (Vector3.New 2. 2. 2.)
       ]

    let world = 
        [
            Plane(UniformSurface <| Matte Blue, 0., Vector3.New 0. 1. 0.).AsShape
            Sphere(UniformSurface <| Matte Red, Vector3.New 1. 1. 1., 1.).AsShape
        ]

    let eye         = Vector3.New 5. 1. 1.
    let viewPort    = ViewPort.New (Vector3.New 4. 0. 0.) (Vector3.New 6. 2. 2.)

    



    0
