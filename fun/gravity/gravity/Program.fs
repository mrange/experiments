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

open GravitySucks

open Gravity

open System

// TODO: Draw persistent lines after particles, color and width depending on mass

type Random with
    member x.NextFloat (inclusiveFrom : float) (inclusiveTo : float) =
        let n = x.NextDouble ()
        inclusiveFrom + n * (inclusiveTo - inclusiveFrom)

[<EntryPoint>]
let main argv = 

    let random  = Random (19740531)
    
    let center  = Particle.New 1000000.F (V2 0.F 0.F) (V2 0.F 0.F)


    let particles =
        [|
            for i in 0..30 do
                if i = 0 then yield center
                else
                    let cm  = center.Mass
                    let m   = random.NextFloat 1000.  10000.
                    let r   = random.NextFloat 200.   1000.
                    let a   = random.NextFloat 0.     (2.0 * Math.PI)
//                    let a   = Math.PI / 4.0
                    let x   = float32 <| r * sin a
                    let y   = float32 <| r * cos  a
                    let p   = Particle.New (float32 m) (V2 x y) (V2 0.F 0.F)
                    let gf  = float <| center.GravityForce p
                    // cf = mv2 / r
                    let v   = sqrt (r * gf / m)
                    let vx  = float32 <| v * cos a 
                    let vy  = float32 <| - v * sin a 
                    p.Velocity <- V2 vx vy
                    yield p
        |]    

    Window.Show particles
    0
