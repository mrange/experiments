module PerfTest = 
    open System.Diagnostics

    let measurePerformance (count : int) (action : unit -> 'T) =
        
        let sw = Stopwatch()
        sw.Start ()

        let result = action ()

        for i in 2..count do
            ignore <| action ()

        sw.Stop ()

        result, sw.ElapsedMilliseconds

module AsyncTest = 
    let rec fib n =
        async {
          if n < 2L then
            return n
          else
            let! n1 = fib (n-1L)
            let! n2 = fib (n-2L)
            return n1 + n2
        }

    let test () = Async.RunSynchronously (fib 23L)

module OverheadAsyncTest = 

    let async2 = new Overhead.Microsoft.FSharp.Control.AsyncBuilder()

    let rec fib n =
        async2 {
          if n < 2L then
            return n
          else
            let! n1 = fib (n-1L)
            let! n2 = fib (n-2L)
            return n1 + n2
        }

    let test () = Overhead.Microsoft.FSharp.Control.Async.RunSynchronously (fib 23L)

module ControlTest = 

    let async2 = new Overhead.Microsoft.FSharp.Control.AsyncBuilder()

    let rec fib n =
        if n < 2L then n
        else
            let n1 = fib (n-1L)
            let n2 = fib (n-2L)
            n1 + n2

    let test () = fib 23L

[<EntryPoint>]
let main argv = 

    let tests =
        [|
            "control"   , ControlTest.test
            "async"     , AsyncTest.test
            "async2"    , OverheadAsyncTest.test
        |]

    for name, action in tests do
        let v, ms = PerfTest.measurePerformance 10 action

        printfn "%A, result: %A, elapsed: %A" name v ms

    0
