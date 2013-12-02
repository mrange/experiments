
open FolderSize

open System

[<EntryPoint>]
let main argv = 

    let o = Scanner.Start """C:\temp"""

    o |> Observable.add (fun v -> printfn "Folder: %A" v)

    o.Start ()

    ignore <| Console.ReadKey ()

    Window.Show ()

    0
