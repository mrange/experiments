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

open System

open TurtlePower

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let turtleGenerator = OptimizedTreeFractal.Generate 10 250.F
    let turtleGenerator = OtherTreeFractal.Generate 8 250.F
    let turtleGenerator = WavingTreeFractal.Generate 10 250.F
    let turtleGenerator = Box.Generate 500.F
    let turtleGenerator = RecursiveBox.Generate 500.F
    let turtleGenerator = SimpleTreeFractal.Generate 10 250.F
    let turtleGenerator = TreeFractal.Generate 10 250.F
    let turtleGenerator = SimpleBox.Generate 500.F
    
    TurtleWindow.Show turtleGenerator

    0
