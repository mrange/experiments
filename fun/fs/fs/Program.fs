
open FolderSize

open System

[<EntryPoint>]
let main argv = 

    let os,o = FolderTree.BuildPipe <| Scanner.Start """C:\temp"""

    use onExitDisposeSource = OnExit <| fun () -> TryDispose os

    let receiveFolder f     = printfn "%A" f
    let scannerCompleted () = ()
    let scannerError exn    = ()

    use terminator = o |> ObservableEx.terminator receiveFolder scannerCompleted scannerError

    // Window.Show ()

    0
