
open FolderSize

open System

[<EntryPoint>]
let main argv = 

    use o = Scanner.Start """C:\temp"""

    Window.Show o

    0
