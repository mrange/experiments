
open FolderSize

open System

[<EntryPoint>]
let main argv = 

    let os,o = FolderTree.BuildPipe <| Scanner.Start """C:\temp\"""

    use onExitDisposeSource = OnExit <| fun () -> TryDispose os

    let receiveFolder (f : FolderTree.Folder)   = printfn "Folder: TFC: %d, TFS: %d" f.TotalFileCount f.TotalFileSize
    let scannerCompleted ()                     = printfn "Completed"
    let scannerError exn                        = printfn "Exception: %A" exn

    use terminator = o |> ObservableEx.terminator receiveFolder scannerCompleted scannerError

    os.Start ()

    ignore <| Console.ReadKey ()


    // Window.Show ()

    0
