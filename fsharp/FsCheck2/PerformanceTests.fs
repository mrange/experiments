module FsCheck2.PerformanceTests

open System
open System.Diagnostics
open FsCheck2.Core
open FsCheck2.Core.Generator

module GeneratorAlternatives =

  let count = 100000

  module Alternative1 =
    type Arbitrary        = Arbitrary of unit
    type GeneratorContext = (struct (Arbitrary*int*RandomGenerator))
    type Generator<'T>    = GeneratorContext -> 'T

    let gint : Generator<int> = 
      fun struct (arb, size, rg) ->
        StdGen.range 0 size rg

    let inline gpair (l : Generator<'L>) (r : Generator<'R>) : Generator<struct ('L*'R)> =
      fun struct (arb, size, rg) ->
        let struct (lrg, rrg) = StdGen.split rg
        let lv = l (struct (arb, size, lrg))
        let rv = r (struct (arb, size, rrg))
        struct (lv, rv)

    let inline gmap (m : 'T -> 'U) (t : Generator<'T>) : Generator<'U> =
      fun ctx ->
        let tv = t ctx
        m tv

    let ctx = struct (Arbitrary (), 1000, StdGen.create 19740531L)
    let g   = gmap (fun struct (l, r) -> l + r) (gpair gint gint)

    let testRun () =
      for i = 1 to count do
        g ctx |> ignore

  module Alternative2 =
    type Arbitrary        = Arbitrary of unit
    type GeneratorContext = (struct (Arbitrary*int*RandomGenerator))
    type Generator<'T>    = GeneratorContext -> struct (RandomGenerator*'T)

    let gint : Generator<int> = 
      fun struct (arb, size, rg) ->
        struct (rg, StdGen.range 0 size rg)

    let inline gpair (l : Generator<'L>) (r : Generator<'R>) : Generator<struct ('L*'R)> =
      fun ctx ->
        let struct (arb, size, _) = ctx
        let struct (rg, lv)       = l ctx
        let struct (rg, rv)       = r (struct (arb, size, rg))
        struct (rg, struct (lv, rv))

    let inline gmap (m : 'T -> 'U) (t : Generator<'T>) : Generator<'U> =
      fun ctx ->
        let struct (trg, tv) = t ctx
        struct (trg, m tv)

    let ctx = struct (Arbitrary (), 1000, StdGen.create 19740531L)
    let g   = gmap (fun struct (l, r) -> l + r) (gpair gint gint)

    let testRun () =
      for i = 1 to count do
        g ctx |> ignore

  module Alternative3 =
    type Arbitrary        = Arbitrary of unit
    type [<AbstractClass>] Generator<'T>() =
      class
        abstract Generate : Arbitrary -> int -> RandomGenerator -> struct (RandomGenerator*'T)
      end

    let gint : Generator<int> =
      { new Generator<_>() with
        member x.Generate arb size rg = 
          struct (rg, StdGen.range 0 size rg) 
      }

    let inline gpair (l : Generator<'L>) (r : Generator<'R>) : Generator<struct ('L*'R)> =
      { new Generator<_>() with
        member x.Generate arb size rg = 
          let struct (rg, lv) = l.Generate arb size rg
          let struct (rg, rv) = r.Generate arb size rg
          struct (rg, struct (lv, rv))
      }

    let inline gmap (m : 'T -> 'U) (t : Generator<'T>) : Generator<'U> =
      { new Generator<_>() with
        member x.Generate arb size rg = 
          let struct (trg, tv) = t.Generate arb size rg
          struct (trg, m tv)
      }

    let ctx = struct (Arbitrary (), 1000, StdGen.create 19740531L)
    let g   = gmap (fun struct (l, r) -> l + r) (gpair gint gint)

    let testRun () =
      let struct (arb, size, rg) = ctx
      for i = 1 to count do
        g.Generate arb size rg |> ignore

  module Alternative4 =
    type Arbitrary        = Arbitrary of unit
    type GeneratorResult<'T>(rg: RandomGenerator, v: 'T) =
      struct
        member x.RandomGenerator  = rg
        member x.Value            = v
      end

    let inline gresult rg v = GeneratorResult<_>(rg, v)

    type [<AbstractClass>] Generator<'T>() =
      class
        abstract Generate : Arbitrary -> int -> RandomGenerator -> GeneratorResult<'T>
      end

    let gint : Generator<int> =
      { new Generator<_>() with
        member x.Generate arb size rg = 
          gresult rg (StdGen.range 0 size rg)
      }

    let inline gpair (l : Generator<'L>) (r : Generator<'R>) : Generator<struct ('L*'R)> =
      { new Generator<_>() with
        member x.Generate arb size rg = 
          let lr = l.Generate arb size rg
          let rr = r.Generate arb size lr.RandomGenerator
          gresult rr.RandomGenerator (struct (lr.Value, rr.Value))
      }

    let inline gmap (m : 'T -> 'U) (t : Generator<'T>) : Generator<'U> =
      { new Generator<_>() with
        member x.Generate arb size rg = 
          let tr = t.Generate arb size rg
          gresult tr.RandomGenerator (m tr.Value)
      }

    let ctx = struct (Arbitrary (), 1000, StdGen.create 19740531L)
    let g   = gmap (fun struct (l, r) -> l + r) (gpair gint gint)

    let testRun () =
      let struct (arb, size, rg) = ctx
      for i = 1 to count do
        g.Generate arb size rg |> ignore


module RandomGenerators =
  let count           = 100000
  let random          = Random ()
  let randomGenerator = StdGen.createFromCurrentTime ()

  let testRandomNext () =
    for i = 1 to count do
      random.Next () |> ignore
        
  let testRandomGeneratorNext () =
    let rec loop rg rem =
      if rem > 0 then
        loop (StdGen.next rg) (rem - 1)
    loop randomGenerator count

  let testRandomGeneratorSplit () =
    let rec loop rg rem =
      if rem > 0 then
        let struct (f, _)  = StdGen.split rg
        loop f (rem - 1)
    loop randomGenerator count

module Generators =
  let count           = 100000
  let randomGenerator = StdGen.createFromCurrentTime ()

  module Simple =
    let arb     = defaultArbritrary ()
    let gint    = arb.Generator<int> ()
    let run ()  =
      let rec loop rg rem =
        if rem > 0 then
          let r = gint.Generate arb 100 randomGenerator
          loop r.RandomGenerator (rem - 1)
      loop randomGenerator count

  module Tuple =
    let arb     = defaultArbritrary ()
    let gtuple  = arb.Generator<int*int*int> ()
    let run ()  =
      let rec loop rg rem =
        if rem > 0 then
          let r = gtuple.Generate arb 100 randomGenerator
          loop r.RandomGenerator (rem - 1)
      loop randomGenerator count

  [<Struct>]
  type MyRecord =
    {
      X : int
      Y : int
      Z : int
    }
    static member New x y z = { X = x; Y = y; Z = z }

  module Record =
    let arb     = defaultArbritrary ()
    let gtuple  = arb.Generator<MyRecord> ()

    let run () =
      let rec loop rg rem =
        if rem > 0 then
          let r = gtuple.Generate arb 100 randomGenerator
          loop r.RandomGenerator (rem - 1)
      loop randomGenerator count

  module SingleElementRecord =
    [<Struct>]
    type MyRecord =
      {
        X : int
      }

    let arb     = defaultArbritrary ()
    let gtuple  = arb.Generator<MyRecord> ()

    let run () =
      let rec loop rg rem =
        if rem > 0 then
          let r = gtuple.Generate arb 100 randomGenerator
          loop r.RandomGenerator (rem - 1)
      loop randomGenerator count

  module Applicative = 
    let arb     = defaultArbritrary ()
    gpure MyRecord.New <*> gint <*> gint <*> gint |> arb.Register
    let gtuple  = arb.Generator<MyRecord> ()

    let run () =
      let rec loop rg rem =
        if rem > 0 then
          let r = gtuple.Generate arb 100 randomGenerator
          loop r.RandomGenerator (rem - 1)
      loop randomGenerator count

  module HandCrafted =
    let arb     = defaultArbritrary ()
    { new Generator<MyRecord>() with 
        override x.Generate arb size rg =
          let tr0 = gint.Generate arb size rg
          let tr1 = gint.Generate arb size tr0.RandomGenerator
          let tr2 = gint.Generate arb size tr1.RandomGenerator
          let mr  = MyRecord.New tr0.Value tr1.Value tr2.Value
          gresult tr2.RandomGenerator mr
    } |> arb.Register
    let gtuple  = arb.Generator<MyRecord> ()

    let run () =
      let rec loop rg rem =
        if rem > 0 then
          let r = gtuple.Generate arb 100 randomGenerator
          loop r.RandomGenerator (rem - 1)
      loop randomGenerator count

// now () returns current time in milliseconds since start
let now : unit -> int64 =
  let sw = Stopwatch ()
  sw.Start ()
  fun () -> sw.ElapsedMilliseconds

// time estimates the time 'action' repeated a number of times
let time repeat action =
  let inline cc i       = GC.CollectionCount i

  let v                 = action ()

  GC.Collect (2, GCCollectionMode.Forced, true)

  let bcc0, bcc1, bcc2  = cc 0, cc 1, cc 2
  let b                 = now ()

  for i in 1..repeat do
    action () |> ignore

  let e = now ()
  let ecc0, ecc1, ecc2  = cc 0, cc 1, cc 2

  v, (e - b), ecc0 - bcc0, ecc1 - bcc1, ecc2 - bcc2

let run () =
  let testCases =
    [|
      //"Generator.Alternative1"          , 100 , GeneratorAlternatives.Alternative1.testRun
      //"Generator.Alternative2"          , 100 , GeneratorAlternatives.Alternative2.testRun
      //"Generator.Alternative3"          , 100 , GeneratorAlternatives.Alternative3.testRun
      //"Generator.Alternative4"          , 100 , GeneratorAlternatives.Alternative4.testRun
      //"System.Random.Next"              , 100 , RandomGenerators.testRandomNext
      //"RandomGenerator.next"            , 100 , RandomGenerators.testRandomGeneratorNext
      //"RandomGenerator.split"           , 100 , RandomGenerators.testRandomGeneratorSplit
      //"Generator.Simple"                , 10  , Generators.Simple.run
      //"Generator.Tuple"                 , 10  , Generators.Tuple.run
      "Generator.Record"                , 10  , Generators.Record.run
      //"Generator.SingleElementRecord"   , 10  , Generators.SingleElementRecord.run
      //"Generator.Applicative"           , 10  , Generators.Applicative.run
      "Generator.HandCrafted"           , 10  , Generators.HandCrafted.run
    |]
    
  for nm, n, a in testCases do
    printfn "Running test case %s %d times..." nm n
    let v, ms, cc0, cc1, cc2 = time n a
    printfn "  it took %d ms, cc (%d, %d, %d)" ms cc0 cc1 cc2
