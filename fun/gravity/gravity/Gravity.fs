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

open SharpDX

open System

module Gravity =
    
    let M = 10.F

    type Particle =
        {
            Mass                : float32 
            Radius              : float32
            mutable Current     : Vector2
            mutable Velocity    : Vector2
        }

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
            }

    [<Struct>]
    type RenderParticle (radius : float32, current : Vector2) =

        member x.Radius     = radius
        member x.Current    = current

    let ApplyGravity (timeStep : float32) (particles : Particle []) =
        let last = particles.Length - 1
        for o in 0..last do
            for i in o + 1..last do
                let outer       = particles.[o]
                let inner       = particles.[i]
                let diff        = outer.Current - inner.Current
                diff.Normalize ()
                let force       = outer.GravityForce inner
                outer.Velocity  <-outer.Velocity - timeStep * force*diff / outer.Mass
                inner.Velocity  <-inner.Velocity + timeStep * force*diff / inner.Mass

    let ApplyInertia (timeStep : float32) (particles : Particle []) = 
        let last = particles.Length - 1
        for p in 0..last do
            let particle        = particles.[p]
            let np              = particle.Current + timeStep * particle.Velocity
            particle.Current    <- np

    let ApplyCollision (timeStep : float32) (particles : Particle []) = 
        let last = particles.Length - 1
        for o in 0..last do
            for i in o + 1..last do
                let outer       = particles.[o]
                let inner       = particles.[i]
                let diff        = outer.Current - inner.Current
                let length      = diff.Length ()
                let d           = (outer.Radius + inner.Radius) - length
                if d > 0.F then
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


    let ToRenderParticles (particles : Particle []) =
        [|
            for p in particles -> RenderParticle(p.Radius, p.Current)
        |]

    let TimeStep (timeStep : float32) (particles : Particle array) =
        ApplyGravity    timeStep particles
        ApplyInertia    timeStep particles
        ApplyCollision  timeStep particles
        ToRenderParticles particles



