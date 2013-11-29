
open System

open TurtlePower

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let turtleGenerator = Box.Generate 500.F
    let turtleGenerator = RecursiveBox.Generate 500.F
    let turtleGenerator = TreeFractal.Generate 10 250.F

    TurtleWindow.Show turtleGenerator

    0
