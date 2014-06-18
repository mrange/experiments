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
    
    let M = 0.01F

    type Particle =
        {
            Mass                : float32 
            Radius              : float32
            mutable Current     : Vector2
            mutable Previous    : Vector2
        }
        static member inline New m c v = 
            {
                Mass        = m 
                Radius      = float32 <| Math.Pow (float m, 1.0/3.0)
                Current     = c 
                Previous    = c - v
            }

    [<Struct>]
    type RenderParticle (radius : float32, current : Vector2) =

        member x.Radius     = radius
        member x.Current    = current

    let ApplyGravity (particles : Particle []) =
        let last = particles.Length - 1
        for o in 0..last do
            for i in o + 1..last do
                let outer       = particles.[o]
                let inner       = particles.[i]
                let diff        = outer.Current - inner.Current
                let ndiff       = diff
                ndiff.Normalize ()
                let lsq         = diff.LengthSquared ()
                let force       = M*outer.Mass*inner.Mass / lsq
                outer.Current   <-outer.Current - force*ndiff / outer.Mass
                inner.Current   <-inner.Current + force*ndiff / inner.Mass

    let ApplyInertia (particles : Particle []) = 
        let last = particles.Length - 1
        for p in 0..last do
            let particle        = particles.[p]
            let np              = particle.Current + particle.Current - particle.Previous
            particle.Previous   <- particle.Current
            particle.Current    <- np

    let ToRenderParticles (particles : Particle []) =
        [|
            for p in particles -> RenderParticle(p.Radius, p.Current)
        |]

    let TimeStep (particles : Particle array) =
        ApplyGravity particles
        ApplyInertia particles
        ToRenderParticles particles



