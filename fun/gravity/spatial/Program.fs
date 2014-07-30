open SharpDX
open System

type Bounds = RectangleF

exception TodoException

module Spatial = 
    [<Literal>] 
    let ChildrenInJunction  = 4
    [<Literal>] 
    let ChildrenInLeaf      = 8

    let inline CutBounds (b : Bounds) : Bounds*Bounds*Bounds*Bounds =
        let inline cb x y   = Bounds(x, y, b.Width / 2.F, b.Height / 2.F)
        let c               = b.Center
        cb b.X b.Y  ,   // 00
        cb c.X b.Y  ,   // 10
        cb b.X c.Y  ,   // 01
        cb c.X c.Y      // 11


open Spatial

[<AllowNullLiteral>]
[<AbstractClass>]
type ISpatialObject =
    abstract Pos        : Vector2
    abstract Location   : int*QuadLeaf  with get,set

and [<AllowNullLiteral>] [<AbstractClass>] QuadTree(bounds : Bounds) as this = 

    [<DefaultValue>] val mutable childCount : int
    [<DefaultValue>] val mutable center     : Vector2
    [<DefaultValue>] val mutable parent     : QuadJunction

    do 
        this.center <- bounds.Center

    member inline x.ChildCount  
        with get () = x.childCount
        and  set v  = x.childCount <- v

    member inline x.Bounds      = bounds
    member inline x.Center      = x.center

    member inline x.Parent
        with get () = x.parent
        and  set v  = x.parent <- v

    abstract Add    : Vector2*ISpatialObject    -> QuadTree
    abstract Update : unit                      -> unit
    abstract Trim   : unit                      -> QuadTree

and [<AllowNullLiteral>] [<Sealed>] QuadJunction(b : Bounds) = 
    inherit QuadTree(b)
    
    [<DefaultValue>] val mutable c00 : QuadTree
    [<DefaultValue>] val mutable c10 : QuadTree
    [<DefaultValue>] val mutable c01 : QuadTree
    [<DefaultValue>] val mutable c11 : QuadTree

    new (b,l00,l10,l01,l11) as x =
        QuadJunction(b) then
        x.c00 <- l00
        x.c10 <- l10
        x.c01 <- l01
        x.c11 <- l11
        x.c00.Parent <- x
        x.c10.Parent <- x
        x.c01.Parent <- x
        x.c11.Parent <- x

    override x.Add (p : Vector2, so : ISpatialObject) =
        let c = x.Center

        if p.X < c.X && p.Y < c.X then
            x.c00 <- x.c00.Add (p,so)
        elif p.Y < c.Y then
            x.c10 <- x.c10.Add (p,so)
        elif p.X < c.X then
            x.c01 <- x.c01.Add (p,so)
        else
            x.c11 <- x.c11.Add (p,so)

        x.ChildCount <- x.ChildCount + 1

        upcast x

    override x.Update () = 
        if x.ChildCount > 0 then
            x.c00.Update ()
            x.c10.Update ()
            x.c01.Update ()
            x.c11.Update ()

    override x.Trim () = 
        if x.ChildCount > 0 then
            x.c00 <- x.c00.Trim ()
            x.c10 <- x.c10.Trim ()
            x.c01 <- x.c01.Trim ()
            x.c11 <- x.c11.Trim ()

            upcast x
        else
            upcast QuadLeaf(x.Bounds)

and [<AllowNullLiteral>] [<Sealed>] QuadLeaf(b : Bounds) =
    inherit QuadTree(b)

    [<DefaultValue>] val mutable so0 : ISpatialObject
    [<DefaultValue>] val mutable so1 : ISpatialObject
    [<DefaultValue>] val mutable so2 : ISpatialObject
    [<DefaultValue>] val mutable so3 : ISpatialObject
    [<DefaultValue>] val mutable so4 : ISpatialObject
    [<DefaultValue>] val mutable so5 : ISpatialObject
    [<DefaultValue>] val mutable so6 : ISpatialObject
    [<DefaultValue>] val mutable so7 : ISpatialObject

    member private x.Child 
        with get i : ISpatialObject =
            match i with
            | 0 -> x.so0
            | 1 -> x.so1
            | 2 -> x.so2
            | 3 -> x.so3
            | 4 -> x.so4
            | 5 -> x.so5
            | 6 -> x.so6
            | 7 -> x.so7
            | _ -> raise <| IndexOutOfRangeException()
        and set i v =
            match i with
            | 0 -> x.so0 <- v
            | 1 -> x.so1 <- v
            | 2 -> x.so2 <- v
            | 3 -> x.so3 <- v
            | 4 -> x.so4 <- v
            | 5 -> x.so5 <- v
            | 6 -> x.so6 <- v
            | 7 -> x.so7 <- v
            | _ -> raise <| IndexOutOfRangeException()


    override x.Add (p : Vector2, so : ISpatialObject) =
        let cnt = x.ChildCount
        if cnt < ChildrenInLeaf then
            let b = x.Bounds
            let c = x.Center
            let b00,b10,b01,b11 = CutBounds b
            let l00,l10,l01,l11 = QuadLeaf(b00),QuadLeaf(b10),QuadLeaf(b01),QuadLeaf(b11)

            for i in 0..ChildrenInLeaf - 1 do
                let so = x.Child i
                if so <> null then
                    let p = so.Pos
                    if p.X < c.X && p.Y < c.X then
                        ignore <| l00.Add (p,so)
                    elif p.Y < c.Y then
                        ignore <| l10.Add (p,so)
                    elif p.X < c.X then
                        ignore <| l01.Add (p,so)
                    else
                        ignore <| l11.Add (p,so)

            let j = QuadJunction(b,l00,l10,l01,l11)                               
            j.Parent <- x.Parent
            j.Add (p,so)
        else
            so.Location     <- cnt,x
            x.Child cnt     <- so
            x.ChildCount    <- cnt + 1
            upcast x

    override x.Update () = 
        let b               = x.Bounds
        let cnt             = x.ChildCount
        let mutable e       = cnt
        let mutable i       = 0
        while i < e do
            let so = x.Child i
            if not <| b.Contains so.Pos then
                let last        = e - 1
                x.Child i       <- x.Child last
                x.Child last    <- so
                e               <- last

        x.ChildCount <- e
        
        while e < cnt do
            let so              = x.Child e
            let p               = so.Pos
            let mutable parent  = x.Parent

            while parent <> null && not <| parent.Bounds.Contains p do
                parent <- x.Parent

            if parent <> null then            
                ignore <| parent.Add (p,so)            
            else
                raise TodoException
                
            e <- e + 1

    override x.Trim () = 
        upcast x

type QuadRoot(b : Bounds) as this= 
    [<DefaultValue>] val mutable tree : QuadTree

    do
        this.tree <- QuadLeaf(b : Bounds)


    let leaf (b : Bounds) : QuadTree = upcast QuadLeaf(b)


    member private x.ExpandBounds (p : Vector2) =
        let b = x.tree.Bounds
        if not <| b.Contains p then
            let inline cb x y   = Bounds(x, y, 2.F * b.Width , 2.F * b.Height)
            let c               = x.tree.Center
            let b,l00,l10,l01,l11 = 
                if p.X < c.X && p.Y < c.X then
                    let b = cb (b.X - b.Width) (b.Y - b.Height)
                    let b00,b10,b01,_ = CutBounds b
                    b,leaf b00,leaf b10,leaf b01,x.tree
                elif p.Y < c.Y then
                    let b = cb b.X (b.Y - b.Height)
                    let b00,b10,_,b11 = CutBounds b
                    b,leaf b00,leaf b10,x.tree,leaf b11
                elif p.X < c.X then
                    let b = cb (b.X - b.Width) b.Y
                    let b00,_,b01,b11 = CutBounds b
                    b,leaf b00,x.tree,leaf b01,leaf b11
                else
                    let b = cb b.X b.Y
                    let _,b10,b01,b11 = CutBounds b
                    b,x.tree,leaf b10,leaf b01,leaf b11
            x.tree <- QuadJunction (b,l00,l10,l01,l11)
            x.ExpandBounds p

    member x.Add (so : ISpatialObject) =
        if so <> null then
            let p = so.Pos
            let b = x.tree.Bounds
            if not <| b.Contains p then
                x.ExpandBounds p
            x.tree <- x.tree.Add (p, so)


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    0 // return an integer exit code

(*
[<Struct>] 
type SpatialObject<'T>(bounds : Bounds, value : 'T) = 
    
    [<DefaultValue>]
    val mutable parent : SpatialTree<'T>

    member x.Bounds = bounds
    member x.Value  = value
    member x.Parent
        with get () = x.parent
        and  set v  = x.parent <- v 

and [<AbstractClass>] SpatialTree<'T>() = 
    
    let mutable bounds = Bounds.Empty
    let mutable parent = Unchecked.defaultof<SpatialTree<'T>>

    member x.Bounds = bounds
    member x.Parent = parent

    member x.UnionBounds (b : Bounds) = 
        bounds <- Bounds.Union (bounds, b)

    abstract Add : SpatialObject<'T> -> SpatialTree<'T>

and [<Sealed>] SpatialGroup<'T>() = 
    inherit SpatialTree<'T>()    

    let children = ResizeArray<SpatialObject<'T>> (ChildrenInGroup)

    override x.Add (child : SpatialObject<'T>) : SpatialTree<'T> =
        if children.Count < ChildrenInGroup then
            children.Add child
            x.UnionBounds child.Bounds
            upcast x
        else
            // TODO: Split
            upcast x

and [<Sealed>] SpatialJunction<'T>() = 
    inherit SpatialTree<'T>()    

    let children = ResizeArray<SpatialTree<'T>> (ChildrenInJunction)

    override x.Add (child : SpatialObject<'T>) : SpatialTree<'T> =
        if children.Count < ChildrenInGroup then
            children.Add child
            x.UnionBounds child.Bounds
            upcast x
        else
            // TODO: Split
            upcast x
*)
