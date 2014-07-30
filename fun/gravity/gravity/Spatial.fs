// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
// If you cannot locate the  Microsoft Public License, please send an email to
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

namespace GravitySucks

module Spatial = 
    open SharpDX
    open System
    open System.Linq

    type Bounds = RectangleF

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


    [<AllowNullLiteral>]
    type ISpatialObject =
        abstract Mass       : float32
        abstract Pos        : Vector2
        abstract Location   : int*QuadLeaf  with get,set

    and [<AllowNullLiteral>] [<AbstractClass>] QuadTree(bounds : Bounds) as this = 
        [<DefaultValue>] val mutable internal childCount : int
        [<DefaultValue>] val mutable internal center     : Vector2
        [<DefaultValue>] val mutable internal parent     : QuadJunction
        [<DefaultValue>] val mutable internal totalMass  : float32

        do 
            this.center <- bounds.Center

        member inline x.TotalMass = x.totalMass

        member inline x.ChildCount  
            with get () = x.childCount
            and  set v  = x.childCount <- v

        member inline x.Bounds      = bounds
        member inline x.Center      = x.center

        member inline x.Parent
            with get () = x.parent

        abstract Add    : float32*Vector2*ISpatialObject    -> QuadTree
        abstract Update : unit                              -> unit
        abstract Trim   : unit                              -> QuadTree

    and [<AllowNullLiteral>] [<Sealed>] QuadJunction(b : Bounds) = 
        inherit QuadTree(b)
    
        [<DefaultValue>] val mutable private c00 : QuadTree
        [<DefaultValue>] val mutable private c10 : QuadTree
        [<DefaultValue>] val mutable private c01 : QuadTree
        [<DefaultValue>] val mutable private c11 : QuadTree

        new (b,l00,l10,l01,l11) as x =
            QuadJunction(b) then
            x.c00           <- l00
            x.c10           <- l10
            x.c01           <- l01
            x.c11           <- l11
            x.c00.parent    <- x
            x.c10.parent    <- x
            x.c01.parent    <- x
            x.c11.parent    <- x
            x.totalMass     <- x.c00.totalMass + x.c10.totalMass + x.c01.totalMass + x.c11.totalMass

        override x.Add (m : float32, p : Vector2, so : ISpatialObject) =
            let c       = x.center

            if p.X < c.X && p.Y < c.X then
                x.c00   <- x.c00.Add (m,p,so)
            elif p.Y < c.Y then
                x.c10   <- x.c10.Add (m,p,so)
            elif p.X < c.X then
                x.c01   <- x.c01.Add (m,p,so)
            else
                x.c11   <- x.c11.Add (m,p,so)

            x.childCount<- x.childCount + 1

            x.totalMass <- x.totalMass + m

            upcast x

        override x.Update () = 
            if x.childCount > 0 then
                x.c00.Update ()
                x.c10.Update ()
                x.c01.Update ()
                x.c11.Update ()

        override x.Trim () = 
            if x.childCount > 0 then
                x.c00 <- x.c00.Trim ()
                x.c10 <- x.c10.Trim ()
                x.c01 <- x.c01.Trim ()
                x.c11 <- x.c11.Trim ()

                upcast x
            else
                upcast QuadLeaf(x.Bounds)

    and [<AllowNullLiteral>] [<Sealed>] QuadLeaf(b : Bounds) =
        inherit QuadTree(b)

        [<DefaultValue>] val mutable private so0 : ISpatialObject
        [<DefaultValue>] val mutable private so1 : ISpatialObject
        [<DefaultValue>] val mutable private so2 : ISpatialObject
        [<DefaultValue>] val mutable private so3 : ISpatialObject
        [<DefaultValue>] val mutable private so4 : ISpatialObject
        [<DefaultValue>] val mutable private so5 : ISpatialObject
        [<DefaultValue>] val mutable private so6 : ISpatialObject
        [<DefaultValue>] val mutable private so7 : ISpatialObject

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

        member x.Object i : #ISpatialObject = downcast x.Child i 

        override x.Add (m : float32, p : Vector2, so : ISpatialObject) =
            let cnt = x.childCount
            if cnt < ChildrenInLeaf then
                let b = x.Bounds
                let c = x.center
                let b00,b10,b01,b11 = CutBounds b
                let l00,l10,l01,l11 = QuadLeaf(b00),QuadLeaf(b10),QuadLeaf(b01),QuadLeaf(b11)

                for i in 0..ChildrenInLeaf - 1 do
                    let so = x.Child i
                    if so <> null then
                        let p = so.Pos
                        if p.X < c.X && p.Y < c.X then
                            ignore <| l00.Add (m, p,so)
                        elif p.Y < c.Y then
                            ignore <| l10.Add (m, p,so)
                        elif p.X < c.X then
                            ignore <| l01.Add (m, p,so)
                        else
                            ignore <| l11.Add (m, p,so)

                let j = QuadJunction(b,l00,l10,l01,l11)                               
                j.parent <- x.parent
                j.Add (m, p,so)
            else
                so.Location     <- cnt,x
                x.Child cnt     <- so
                x.childCount    <- cnt + 1
                x.totalMass     <- x.totalMass + m
                upcast x

        member internal x.PreRemove (i : int) = 
            let so                  = x.Child i
            if so <> null then
                let m               = so.Mass
                let p               = so.Pos
                let mutable parent  = x.parent
                x.totalMass         <- x.totalMass - m

                while parent <> null && not <| parent.Bounds.Contains p do
                    parent.childCount   <- x.childCount - 1
                    parent.totalMass    <- x.totalMass - m
                    parent              <- x.parent

        member internal x.PostRemove () = 
            let cnt         = x.childCount            
            let mutable e   = cnt
            let mutable i   = e

            while i < e do
                let so = x.Child i
                if so = null then
                    let last        = e - 1
                    let l           = x.Child last
                    if l <> null then
                        l.Location  <- i,x
                    x.Child i       <- x.Child last
                    x.Child last    <- so
                    e               <- last
                else
                    i <- i + 1

            x.childCount <- i

        override x.Update () = 
            let b               = x.Bounds
            let cnt             = x.childCount
            let mutable e       = cnt
            let mutable i       = 0
            while i < e do
                let so = x.Child i
                if not <| b.Contains so.Pos then
                    let last        = e - 1
                    let l           = x.Child last
                    so.Location     <- -1,null
                    l.Location      <- i,x
                    x.Child i       <- x.Child last
                    x.Child last    <- so
                    e               <- last
                else
                    i <- i + 1

            x.childCount <- e
        
            while e < cnt do
                let so              = x.Child e
                let m               = so.Mass
                let p               = so.Pos
                let mutable parent  = x.parent

                x.totalMass         <- x.totalMass - m

                while parent <> null && not <| parent.Bounds.Contains p do
                    parent.childCount   <- x.childCount - 1
                    parent.totalMass    <- x.totalMass - m
                    parent              <- x.parent

                if parent <> null then            
                    ignore <| parent.Add (m, p,so)            
                else
                    raise TodoException
                
                e <- e + 1

        override x.Trim () = 
            upcast x

    type QuadRoot<'T when 'T :> ISpatialObject>(b : Bounds) as this= 
        [<DefaultValue>] val mutable private tree   : QuadTree
        [<DefaultValue>] val mutable private objects: ResizeArray<'T>

        do
            this.tree       <- QuadLeaf(b : Bounds)
            this.objects    <- ResizeArray<'T>(32)

        let leaf (b : Bounds) : QuadTree = upcast QuadLeaf(b)

        member x.ObjectLength   = x.objects.Count
        member x.Object i       = x.objects.[i]

        member private x.ExpandBounds (p : Vector2) =
            let b = x.tree.Bounds
            if not <| b.Contains p then
                let inline cb x y   = Bounds(x, y, 2.F * b.Width , 2.F * b.Height)
                let c               = x.tree.center
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

        member x.Add (so : 'T) =
            if not <| obj.ReferenceEquals (so, null) then
                let m = so.Mass
                let p = so.Pos
                let b = x.tree.Bounds
                if not <| b.Contains p then
                    x.ExpandBounds p
                x.tree <- x.tree.Add (m, p, so)
                x.objects.Add so

        member x.InplaceFilter (filter : 'T -> bool) =
            let objs        = x.objects
            let cnt         = objs.Count 
            let mutable e   = cnt
            let mutable i   = 0

            while i < e do
                let o = objs.[i]
                if not <| filter o then
                    let last = e - 1
                    objs.[i]    <- objs.[last]
                    objs.[i]    <- o
                    e           <- last
                else
                    i <- i + 1

            let leafs = ResizeArray<QuadLeaf>(cnt - i)

            while i < cnt do
                let o       = objs.[i]
                let i,l     = o.Location
                leafs.Add l
                l.PreRemove i
                e <- e + 1

            let leafs = leafs.Distinct().ToArray()
            for leaf in leafs do
                leaf.PostRemove()

            objs.RemoveRange (e, cnt - e)

        member x.Update () = 
            x.tree.Update ()

        member x.Trim () = 
            ignore <| x.tree.Trim ()

