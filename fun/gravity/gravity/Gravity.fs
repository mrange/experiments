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

module Gravity =
    open SharpDX
    open Spatial
    open System

    let M = 10.F

(* Elastic
                    let a = d + 0.F
                    diff.Normalize ()
                    let sum         = outer.Mass + inner.Mass
                    let op          = outer.Current + a * diff * inner.Mass / sum
                    let ip          = inner.Current - a * diff * outer.Mass / sum
                    outer.Current   <-op
                    inner.Current   <-ip
                    let ov          = outer.Velocity
                    let iv          = inner.Velocity
                    let onv         = ((outer.Mass - inner.Mass) / sum) * ov + ((2.F * inner.Mass) / sum) * iv
                    let inv         = ((inner.Mass - outer.Mass) / sum) * iv + ((2.F * outer.Mass) / sum) * ov
                    let aonv        = 2.F * (diff * onv) * diff - onv
                    let ainv        = 2.F * (diff * inv) * diff - inv
                    outer.Velocity  <- aonv
                    inner.Velocity  <- ainv
*)

    type Particle =
        {
            mutable Mass        : float32 
            mutable Radius      : float32
            mutable Current     : Vector2
            mutable Velocity    : Vector2
            mutable Index       : int
            mutable Leaf        : QuadLeaf
        }

        interface ISpatialObject with
            member x.Pos        = x.Current
            member x.Location  
                with get ()     = x.Index,x.Leaf
                and  set (i,l)  = x.Index <- i; x.Leaf <- l
            

        member x.UpdateRadius ()                = x.Radius <- float32 <| Math.Pow (float x.Mass, 1.0/3.0)

        member x.KineticEnergy                  = let v = x.Velocity
                                                  x.Mass * (v * v) / 2.0F

        member x.Momentum                       = x.Mass * x.Velocity


        member x.GravityForce (p : Particle)    = let diff        = x.Current - p.Current
                                                  let lsq         = diff.LengthSquared ()
                                                  let force       = M*x.Mass*p.Mass / lsq
                                                  force

        member x.PotentialEnergy (p : Particle) = let v = x.Velocity
                                                  x.Mass * (v * v) / 2.0F


        static member inline New m c v = 
            {
                Mass        = m 
                Radius      = float32 <| Math.Pow (float m, 1.0/3.0)
                Current     = c 
                Velocity    = v
                Index       = -1
                Leaf        = null
            }

    [<Struct>]
    type RenderParticle (mass : float32, radius : float32, current : Vector2, velocity : Vector2) =

        member x.Mass       = mass
        member x.Radius     = radius
        member x.Current    = current
        member x.Velocity   = velocity

    type ParticleTree = QuadRoot<Particle>

    let ApplyGravity (timeStep : float32) (tree : ParticleTree) =
        let last = tree.ObjectLength - 1
        for i in 0..last do
            let particle        = tree.Object i
            let leaf            = particle.Leaf
            let last            = leaf.ChildCount - 1

            for i in 0..last do
                let other       = leaf.Object<Particle> i
                if other <> particle then
                    let diff            = particle.Current - other.Current
                    diff.Normalize ()
                    let force           = particle.GravityForce other
                    let v               = timeStep*force*diff
                    particle.Velocity   <-particle.Velocity - v / particle.Mass
                    other.Velocity      <-other.Velocity + v / other.Mass

        tree

    let ApplyInertia (timeStep : float32) (tree : ParticleTree) = 
        let last = tree.ObjectLength - 1
        for i in 0..last do
            let particle        = tree.Object i
            let np              = particle.Current + timeStep * particle.Velocity
            particle.Current    <- np
        tree

    let ApplyCollision (tree : ParticleTree) = 
        let last = particles.Length - 1
        for o in 0..last do
            let outer           = particles.[o]
            let mutable i       = o + 1
            while i <= last && outer.Mass > 0.F do
                let inner       = particles.[i]
                let diff        = outer.Current - inner.Current
                let length      = diff.Length ()
                let d           = (outer.Radius + inner.Radius) - length
                if d > 0.F then
                    let twposition  = outer.Mass * outer.Current + inner.Mass * inner.Current
                    let tmomentum   = outer.Momentum + inner.Momentum
                    let tmass       = outer.Mass + inner.Mass
                    let tposition   = twposition / tmass
                    let tvelocity   = tmomentum / tmass
                    inner.Mass      <- tmass
                    inner.Current   <- tposition
                    inner.Velocity  <- tvelocity
                    inner.UpdateRadius ()
                    outer.Mass      <- 0.F  // Will break the while loop
                i <- i + 1
        particles

    let RemoveMassless (tree : ParticleTree) =
        tree.InplaceFilter <| fun p -> p.Mass > 0.F 
        tree

    let UpdateTree (tree : ParticleTree) = 
        tree.Update ()
        tree.Trim ()
        tree

    let ToRenderParticles (tree : ParticleTree) =
        let length  = tree.ObjectLength
        let rps     = Array.zeroCreate<RenderParticle> length
        for i in 0..length - 1 do
            let p   = tree.Object i
            let rp  = RenderParticle(p.Mass, p.Radius, p.Current, p.Velocity)
            rps.[i] <- rp
        rps

    let Cleanup (tree : ParticleTree) = 
        tree
        |> ApplyCollision
        |> RemoveMassless
        |> UpdateTree

    let TimeStep (timeStep : float32) (tree : ParticleTree) =
        let tree =  tree
                    |> ApplyGravity    timeStep
                    |> ApplyInertia    timeStep 
                    |> ApplyCollision  
                    |> RemoveMassless  
                    |> UpdateTree
        tree, ToRenderParticles tree



