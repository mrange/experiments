open FsCheck2.Core
open System

type MyRecord<'T> =
  {
    X : int
    Y : 'T
    Z : int
  }

type MyGenerator(g0 : Generator<int>, g1 : Generator<string>, g2 : Generator<int>) =
  inherit Generator<int*string*int>()
  override x.Generate arb size rg =
    let tr0 = g0.Generate arb size rg
    let tr1 = g1.Generate arb size tr0.RandomGenerator
    let tr2 = g2.Generate arb size tr1.RandomGenerator
    let mr  = tr0.Value, tr1.Value, tr2.Value
    Generator.gresult tr2.RandomGenerator mr

[<EntryPoint>]
let main argv = 
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

    let arb = defaultArbritrary ()
    let i   = arb.Generator<int> ()
    let s   = arb.Generator<string> ()
    let t   = arb.Generator<int*string*int> ()
//    let r   = arb.Generator<MyRecord<string>> ()
    let rg  = StdGen.create 19740531L
    let gr  = t.Generate arb 100 rg
    printfn "%A" gr.Value
    FsCheck2.PerformanceTests.run ()
    0
  with
  | (ArbritraryException (msg, e)) ->
    printfn "%A" msg
    999
  | e ->
    printfn "%A" e.Message
    999
