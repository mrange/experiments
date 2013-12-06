
open FolderSize

open System

[<EntryPoint>]
let main argv = 

    let os,o = FolderTree.BuildPipe <| Scanner.Start """C:\temp\"""

    use onExitDisposeSource = OnExit <| fun () -> TryDispose os

    Window.Show os o

    0
