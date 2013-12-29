
open FolderSize

open System
open SharpDX

open Units
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
                Label 
                    [ 
                        Margin  .Value <| Thickness.Uniform 4.F
                        Text    .Value "Hi there!" 
                    ]
                TextButton "Click me!"
                    [ 
                    ]
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
