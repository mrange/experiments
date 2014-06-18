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

[<EntryPoint>]
let main argv = 

    let particles = 
        [|
            Particle.New 1000000.F  (V2 0.0F 0.0F)          (V2 0.0F 0.0F)
            Particle.New 1000.F     (V2 300.0F 300.0F)      (V2 -5.0F 1.0F)
            Particle.New 1000.F     (V2 -300.0F -300.0F)    (V2 7.0F 1.0F)
        |]

    Window.Show particles
    0
