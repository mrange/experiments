
open FolderSize

open System
open SharpDX

[<EntryPoint>]
let main argv = 

    let os,o = FolderTree.BuildPipe <| Scanner.Start """C:\Temp\"""

    use onExitDisposeSource = OnExit <| fun () -> TryDispose os

    let myui = 
        ui {
            do! UserInterface.Label (RectangleF(10.F, 10.F, 60.F, 24.F) |> Animated.Constant) "Path:"
            do! UserInterface.Label (RectangleF(10.F, 70.F, 200.F, 24.F) |> Animated.Constant) "C:\temp"

            return UserInterface.ButtonEx
        }

    Window.Show os o

    0
