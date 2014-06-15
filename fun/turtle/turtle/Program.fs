
open System

open TurtlePower

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let turtleGenerator = OptimizedTreeFractal.Generate 10 250.F
    let turtleGenerator = OtherTreeFractal.Generate 8 250.F
    let turtleGenerator = RecursiveBox.Generate 500.F
    let turtleGenerator = SimpleBox.Generate 500.F
    let turtleGenerator = Box.Generate 500.F
    let turtleGenerator = SimpleBox.Generate 500.F
    let turtleGenerator = SimpleTreeFractal.Generate 8 250.F
    let turtleGenerator = TreeFractal.Generate 10 250.F
    let turtleGenerator = WavingTreeFractal.Generate 10 250.F

    TurtleWindow.Show turtleGenerator

    0
