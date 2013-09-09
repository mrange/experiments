namespace RayTracer


[<AbstractClass>]
type Shape (surface: Surface) = 
    member x.Surface with get () = surface
    member x.AsShape with get () = x
    abstract Intersect      : Ray           -> Intersection option
    abstract Intersection   : Intersection  -> IntersectionData
and Intersection =
    {
        Shape       : Shape
        Ray         : Ray
        Distance    : float
    }
    static member New shape ray distance = {Shape = shape; Ray = ray; Distance = distance}
and IntersectionData =
    {
        Intersection: Intersection
        Normal      : Vector3
        Point       : Vector3
        Material    : Material
        Reflect     : Ray
    }
    static member New intersection normal point material = 
        {
            Intersection    = intersection
            Normal          = normal        
            Point           = point 
            Material        = material
            Reflect         = Ray.DirectionOrigin (intersection.Ray.Direction.Reflect normal) point
        }


type LightSource = 
    {
        Color   : Color
        Origin  : Vector3
        Radius  : float
    }
    static member New color origin radius = {Color = color; Origin = origin; Radius = radius}

    member x.Intersect (r : Ray) =
        match r.IntersectSphere x.Origin x.Radius with
        |   None -> None
        |   Some (t1, _) ->
            let p   = r.Trace t1
            let n'  = (p - x.Origin)
            let n   = n'.Normalize
            Some (p, n)

type Sphere (surface: Surface, center : Vector3, radius : float) =
    inherit Shape (surface)

    member x.NormalAndMaterial p =
        let n' = (p - center) 
        let n = n'.Normalize
        let y' = Vector3.New n.X center.Y n.Z
        let y = (sign (n.Y - center.Y)) * acos ((y' * n) / (y'.L1 * n.L1)) / pi2 + 0.5
        let x' = Vector3.New n.X center.Y center.Z
        let x = (sign (n.Z - center.Z)) * acos ((x' * y') / (x'.L1 * y'.L1)) / pi2 + 0.5

        let m = base.Surface <| Vector2.New x y

        n,m

    override x.Intersect r =
        match r.IntersectSphere center radius with
        |   None -> None
        |   Some (t1, _) ->
            Some <| Intersection.New x r t1

    override x.Intersection i =
        let p = i.Ray.Trace i.Distance
        let n,m = x.NormalAndMaterial p
        IntersectionData.New i n p m

type Plane (surface: Surface, offset : float, normal : Vector3)=
    inherit Shape (surface)

    let N = normal.Normalize

    let X = N.ComputeNormal().Normalize

    let Y = N *+* X

    override x.Intersect r = 
        match r.IntersectPlane N offset with
        |   Some t  ->

            Some <| Intersection.New x r t
        |   None    -> None

    override x.Intersection i =
        let p = i.Ray.Trace i.Distance
        let c = Vector2.New (X * p) (Y * p)
        let m = base.Surface c
        IntersectionData.New i N p m

type ViewPort = 
    {
        Center          : Vector3
        Normal          : Vector3
        Axis0           : Vector3
        Axis1           : Vector3
        Width           : float
        Height          : float
        Corner0         : Vector3
        Corner1         : Vector3
        Corner2         : Vector3
        Corner3         : Vector3
    }

    static member New (eye :Vector3) (at : Vector3) (up : Vector3) (clipDistance : float) (fov : double) (ratio : float) = 
        let clipNormal  = (at - eye).Normalize
        let clipCenter  = eye + clipNormal.Scale clipDistance

        let width       = clipDistance * tan (fov / 2.)
        let height      = width * ratio

        let xaxis = (up *+* clipNormal).Normalize
        let yaxis = (clipNormal *+* xaxis).Normalize

        let halfx       = xaxis.Scale (width / 2.)
        let halfy       = yaxis.Scale (height / 2.)

        {
            Center  = clipCenter
            Normal  = clipNormal
            Axis0   = xaxis
            Axis1   = yaxis
            Width   = width
            Height  = height
            Corner0 = clipCenter - halfx - halfy
            Corner1 = clipCenter + halfx - halfy
            Corner2 = clipCenter + halfx + halfy
            Corner3 = clipCenter - halfx + halfy
        }


[<AutoOpen>]
module RayTracerUtil =

    let UniformSurface (material : Material) : Surface = fun v -> material
    let CirclesSurface width (onmaterial : Material) (offmaterial : Material) : Surface = 
        fun v -> 
            let dist = v.L1
            let m = (int (dist / width)) % 2

            if m = 0 then onmaterial
            else offmaterial

    let Matte c = Material.New c 1. 1. 0. 0. White
    let Reflective r c = Material.New c 1. (1. - r) 0. r White

    let Lightning (i : IntersectionData) (shapes : Shape[]) (lights : LightSource[]) =
        let isShapeBlockingLight (light : LightSource) (shape : Shape) = 
            let lightRay = Ray.FromTo i.Point light.Origin
            match shape.Intersect lightRay with
            |   Some _  -> true
            |   _       -> false

        let isLightVisible (light : LightSource) = 
            let someShapesAreBlockingLight = 
                shapes
                |> Array.exists (isShapeBlockingLight light)
            not someShapesAreBlockingLight

        let diffuse (light : LightSource) = 
            let direction = (light.Origin - i.Point).Normalize
            let c = direction * i.Normal
            light.Color.Dim (c * i.Material.Diffusion)

        let specular (light : LightSource) = 
            match light.Intersect i.Reflect with
            | Some (p,n)    -> light.Color.Dim (abs (n*i.Reflect.Direction))
            | _             -> Color.Zero

        let mutable sumOfDiffusion  = Color.Zero
        let mutable sumOfSpecular   = Color.Zero

        for light in lights do
            if isLightVisible light then
                sumOfDiffusion  <- sumOfDiffusion + diffuse light
                sumOfSpecular   <- sumOfSpecular + specular light


        (sumOfDiffusion * i.Material.Color).Dim i.Material.Diffusion, (sumOfSpecular * i.Material.Specular)

            
    let rec TraceImpl (remaining : int) (ray : Ray) (shapes : Shape[]) (lights : LightSource[]) = 
        if remaining < 1 then Color.Zero
        else
        
            let mutable closestIntersection : Intersection option = None
            for shape in shapes do
                let intersection = shape.Intersect ray
                closestIntersection <- 
                    match intersection, closestIntersection with
                    |   Some i, Some ci when i.Distance > ci.Distance 
                                        -> Some ci
                    |   Some i, _       -> Some i
                    |   _               -> closestIntersection

            match closestIntersection with
                |   Some i      ->
                    let id = i.Shape.Intersection i
                    let diffusion, specular = 
                        if id.Material.Diffusion > 0. || id.Material.Specular > Color.Zero then Lightning id shapes lights
                        else Color.Zero, Color.Zero

                    let reflection = 
                        if id.Material.Reflection > 0. then 
                            (TraceImpl (remaining - 1) id.Reflect shapes lights).Dim id.Material.Reflection
                        else Color.Zero

                    diffusion + specular + reflection
                |   _           -> Color.Zero
        
    let Trace (ray : Ray) (world : Shape[]) (lights : LightSource[])=
        TraceImpl 4 ray world lights

