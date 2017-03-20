open FsCheck2.Core

type MyRecord<'T> =
  {
    X : int
    Y : 'T
    Z : int
  }

[<EntryPoint>]
let main argv = 
  try
    let arb = defaultArbritrary ()
    let i   = arb.Generator<int> ()
    let s   = arb.Generator<string> ()
    let t   = arb.Generator<int*string*int> ()
    let r   = arb.Generator<MyRecord<string>> ()
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
