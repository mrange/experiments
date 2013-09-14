namespace RayTracer

type BoundingSphere =
    {
        Center  : Vector3
        Radius  : float
    }
    static member New center radius = {Center = center; Radius = radius}
    member x.Volume = 4. * pi * x.Radius * x.Radius * x.Radius / 4.
    member x.Intersect (r : Ray) = r.IntersectSphere x.Center x.Radius
    member x.Merge (bs : BoundingSphere) = 
        let diff    = (x.Center - bs.Center)
        let radius  = (diff.Length + x.Radius + bs.Radius) / 2.
        let diffn   = diff.Normalize
        let max     = x.Center + diffn * x.Radius
        let center  = max - diffn * radius
        BoundingSphere.New center radius


type RTree<'a> = 
    |   Leaf    of  BoundingSphere*'a
    |   Fork    of  BoundingSphere*RTree<'a>*RTree<'a>
    member x.Cons   () = ()
    member x.Matches (r : Ray) = ()







