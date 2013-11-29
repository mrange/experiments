
open System

open TurtlePower

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let turtleGenerator = Box.Generate 500.F

    TurtleWindow.Show turtleGenerator

    0
