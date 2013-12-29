
open FolderSize

open System
open SharpDX

open Logical
open Logical.Properties

[<EntryPoint>]
let main argv = 

    let os,o = FolderTree.BuildPipe <| Scanner.Start """C:\Temp\"""

    use onExitDisposeSource = OnExit <| fun () -> TryDispose os

    let body = 
        Stack 
            [
                Orientation.Value FromTop
            ]
            [
                Label []
                Label []
                Label []
            ]



//    let myui = 
//        ui {
//            do! UserInterface.Label (RectangleF(10.F, 10.F, 60.F, 24.F) |> Animated.Constant) "Path:"
//            do! UserInterface.Label (RectangleF(10.F, 70.F, 200.F, 24.F) |> Animated.Constant) "C:\temp"
//
//            return UserInterface.ButtonEx
//        }

    Window.Show os o

    0
