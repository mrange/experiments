namespace RayTracer

type BoundingPlane = 
    {
        Normal  : Vector3
        Offset  : float
    }

type BoundingBox = 
    {
        Up          : BoundingPlane
        Down        : BoundingPlane
        North       : BoundingPlane
        South       : BoundingPlane
        East        : BoundingPlane
        West        : BoundingPlane
    }

type OctTree<'a> =
    | Leaf      of 'a list
    | Box       of BoundingBox * Fork<'a>
    member x.FindCandidates (r : Ray) = 
        match x with
        |   Leaf c  -> c
        |   _       -> []
and Fork<'a> =
    {
        UpNorthEast     : OctTree<'a>
        UpNorthWest     : OctTree<'a>
        UpSouthWest     : OctTree<'a>
        UpSouthEast     : OctTree<'a>
        DownNorthEast   : OctTree<'a>
        DownNorthWest   : OctTree<'a>
        DownSouthWest   : OctTree<'a>
        DownSouthEast   : OctTree<'a>
    }
    
        