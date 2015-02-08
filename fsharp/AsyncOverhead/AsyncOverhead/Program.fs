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

[<AutoOpen>]
module Common =
    let Limit = 20L

module Async2Test =
    open System
    open System.Collections.Generic
    open System.Threading

    type AsyncContext(token : CancellationToken) =
        
        let stack = Stack<_>()

        member x.PushExceptionHandler (handler : exn -> unit) : unit =
            stack.Push handler
        

    [<NoEquality>]
    [<NoComparison>]
    [<AbstractClass>]
    type Continuation<'T>() =
        abstract Continue   : 'T                            -> unit

    [<NoEquality>]
    [<NoComparison>]
    [<AbstractClass>]
    type Async<'T>() =
        abstract Build      : AsyncContext*Continuation<'T> -> unit


    module AsyncSupport =
        let inline Delay (ft : unit -> Async<'T>) : Async<'T> = 
            { 
                new Async<'T>() with
                    member x.Build (ctx, c) = 
                        let t = ft ()
                        t.Build (ctx, c)
            }

        let inline Return (v : 'T) : Async<'T> = 
            { 
                new Async<'T>() with
                    member x.Build (ctx, c) = c.Continue v
            }

        
        let inline Bind (t : Async<'T>) (fu : 'T -> Async<'U>) : Async<'U> = 
            let inline tc ctx c = 
                {
                    new Continuation<'T>() with
                        member x.Continue v =
                            let u = fu v
                            u.Build (ctx, c)
                }
            { 
                new Async<'U>() with
                    member x.Build (ctx, c) = 
                        let ic = tc ctx c
                        t.Build (ctx, ic)
            }

    [<Sealed>]
    type AsyncBuilder() =
        member x.Delay(f)       = AsyncSupport.Delay f
        member x.Return(v)      = AsyncSupport.Return v
        member x.Bind(t, fu)    = AsyncSupport.Bind t fu

    module Async =
        let RunSynchronously (t : Async<'T>) : 'T = 
            use cts     = new CancellationTokenSource()
            let ct      = cts.Token
            let result  = ref Unchecked.defaultof<'T>
            let ctx     = AsyncContext ct
            let c       =
                {
                    new Continuation<'T>() with
                        member x.Continue v =
                            result := v
                }

            t.Build (ctx,c)

            !result

    let async2 = AsyncBuilder()        

    let rec fib n =
        async2 {
          if n < 2L then
            return n
          else
            let! n2 = fib (n-2L)
            let! n1 = fib (n-1L)
            return n1 + n2
        }


    let test () = Async.RunSynchronously (fib Limit)
    
module Async3Test =
    open System
    open System.Collections.Generic
    open System.Threading

    type AsyncContext(token : CancellationToken) =
        
        let stack = Stack<_>()

        member x.PushExceptionHandler (handler : exn -> unit) : unit =
            stack.Push handler
        

    type Continuation<'T>   = 'T -> unit
    type Async<'T>          = AsyncContext*Continuation<'T> -> unit

    module AsyncSupport =
        let inline Delay (ft : unit -> Async<'T>) : Async<'T> = 
            fun (ctx, c) ->
                let t = ft ()
                t (ctx, c)

        let inline Return (v : 'T) : Async<'T> = 
            fun (ctx, c) ->
                c v

        
        let inline Bind (t : Async<'T>) (fu : 'T -> Async<'U>) : Async<'U> = 
            fun (ctx, c) ->
                let ic v = 
                    let u = fu v
                    u (ctx, c)

                t (ctx, ic)

    [<Sealed>]
    type AsyncBuilder() =
        member x.Delay(f)       = AsyncSupport.Delay f
        member x.Return(v)      = AsyncSupport.Return v
        member x.Bind(t, fu)    = AsyncSupport.Bind t fu

    module Async =
        let RunSynchronously (t : Async<'T>) : 'T = 
            use cts     = new CancellationTokenSource()
            let ct      = cts.Token
            let result  = ref Unchecked.defaultof<'T>
            let ctx     = AsyncContext ct
            let c v     = result := v

            t (ctx,c)

            !result

    let async2 = AsyncBuilder()        

    let rec fib n =
        async2 {
          if n < 2L then
            return n
          else
            let! n1 = fib (n-1L)
            let! n2 = fib (n-2L)
            return n1 + n2
        }

    let test () = Async.RunSynchronously (fib Limit)

module TaskTest = 

    open System.Threading.Tasks
    
    let rec fib n =
      if n < 2L then n
      else
        let n2t = Task.Factory.StartNew (fun _ -> fib (n-2L))
        let n1 = fib (n-1L)
        n2t.Result + n1
 
    let test () = fib Limit


module AsyncTest = 
(*
    let rec fib n =
        async {
          if n < 2L then
            return n
          else
            let! n1 = fib (n-1L)
            let! n2 = fib (n-2L)
            return n1 + n2
        }
*)

    let rec fib n =
        async {
          if n < 2L then
            return n
          else
            let! n2a = fib (n-2L) |> Async.StartChild
            let! n1 = fib (n-1L)
            let! n2 = n2a
            return n1 + n2
        }

    let test () = Async.RunSynchronously (fib Limit)

module HopacTest =
    open Hopac
    open Hopac.Job.Infixes

    let rec fib n = Job.delay <| fun () ->
      if n < 2L then
        Job.result n
      else
        fib (n-2L) <*> fib (n-1L) |>> fun (x, y) ->
        x + y

    let test () = run (fib Limit)


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

    let test () = Overhead.Microsoft.FSharp.Control.Async.RunSynchronously (fib Limit)

module ControlTest = 

    let async2 = new Overhead.Microsoft.FSharp.Control.AsyncBuilder()

    let rec fib n =
        if n < 2L then n
        else
            let n1 = fib (n-1L)
            let n2 = fib (n-2L)
            n1 + n2

    let test () = fib Limit

open System.Diagnostics
open System.Threading

[<EntryPoint>]
let main argv = 

    let p = Process.GetCurrentProcess ()
    p.ProcessorAffinity <- nativeint 1

    printfn "Awaiting affinity to stick..."
    Thread.Sleep 500

    let tests =
        [|
            "control"   , ControlTest.test
            "async"     , AsyncTest.test
            "task"      , TaskTest.test
            "hopac"     , HopacTest.test
            "asynco"    , OverheadAsyncTest.test
            "async2"    , Async2Test.test
            "async3"    , Async3Test.test
        |]

    for name, action in tests do
        let v, ms = PerfTest.measurePerformance 100 action

        printfn "%A, result: %A, elapsed: %A" name v ms

    0
