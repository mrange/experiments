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
//            1000000.F   , 000.0F    , 000.0F    , 0.0F  , 000.F
//            100000.F    , 300.0F    , 000.0F    , 0.0F  , 100.F
//            100000.F    , 000.0F    , 300.0F    ,-102.F , 000.F
//            100000.F    , 000.0F    ,-300.0F    , 149.F , 000.F
//            100000.F    ,-300.0F    , 000.0F    , 0.0F  ,-148.F
//            100000.F    , 000.0F    ,-300.0F    , 149.F , 000.F
            100000.F    , 100.0F    , 000.0F    , 0.0F  , 0.0F
            100000.F    ,-100.0F    , 000.0F    , 0.0F  , 0.0F
        |] |> Array.map (fun (m,x,y,vx,vy) -> Particle.New m (V2 x y) (1.3F * (V2 vx vy)))

    Window.Show particles
    0
