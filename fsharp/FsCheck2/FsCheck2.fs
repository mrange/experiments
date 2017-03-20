module FsCheck2.Core

open FSharp.Core.Printf
open FSharp.Reflection
open System
open System.Collections.Concurrent
open System.Linq.Expressions
  
type Receiver<'T>     = 'T -> bool
type Stream<'T>       = Receiver<'T> -> unit

exception ArbritraryException of string*exn option

[<Struct>]
type RandomGenerator  = RandomGenerator of int * int

type GeneratorResult<'T>(rg: RandomGenerator, v: 'T) =
  struct
    member x.RandomGenerator  = rg
    member x.Value            = v
  end

and [<AbstractClass>]  Generator () =
  class 
    abstract Generates    : Type
  end

and [<AbstractClass>]  Shrinker () =
  class 
    abstract Shrinks : Type
  end

and [<Sealed>] Assembler (assembles : Type, lambda : LambdaExpression) =
  class 
    member x.Assembles  = assembles
    member x.Lambda     = lambda
  end

and [<Sealed>] Disassembler (disassembles : Type) =
  class 
    member x.Disassembles = disassembles
  end

and [<Sealed>] Arbitrary () =
  class
    // TODO: Use more type-safe reflection
    //  For property and calls too.
    static let rFromLambda          = typeof<Arbitrary>.GetMethod "FromLambda"
    static let rGenerator_          = typeof<Generator<int>>.GetGenericTypeDefinition ()
    static let rGenerator         t = rGenerator_.MakeGenericType [|t|]
    static let rGeneratorResult_    = typeof<GeneratorResult<int>>.GetGenericTypeDefinition ()
    static let rGeneratorResult   t = rGeneratorResult_.MakeGenericType [|t|]
    static let rBreak               = typeof<Diagnostics.Debugger>.GetMethod "Break"


    let generators    = ConcurrentDictionary<Type, Generator    > ()
    let shrinkers     = ConcurrentDictionary<Type, Shrinker     > ()
    let assemblers    = ConcurrentDictionary<Type, Assembler    > ()
    let disassemblers = ConcurrentDictionary<Type, Disassembler > ()

    let raisea  e msg = raise (ArbritraryException (msg, e))
    let raiseaf e fmt = kprintf (raisea e) fmt
 
    let createAssembler = Func<Type, Assembler> (fun t ->
      if FSharpType.IsRecord t || FSharpType.IsTuple t then
        let ctor  = (t.GetConstructors ()).[0]  // TODO: 
        let ps    = ctor.GetParameters ()
        let lps   = ps |> Array.map (fun p -> Expression.Parameter (p.ParameterType, p.Name))
        let ne    = Expression.New (ctor, lps |> Array.map (fun p -> p :> Expression))
        let le    = Expression.Lambda (ne, lps)
        Assembler (t, le)
      else
        raiseaf None "Couldn't find find assembler for type: %s" t.FullName 
      )

    let assembler t =
      assemblers.GetOrAdd (t, createAssembler)

    let rec createGenerator ts = Func<Type, Generator> (fun t ->
      try
        if FSharpType.IsUnion t then
//          let ucs   = FSharpType.GetUnionCases t
          failwith "TODO:"
        else
          let assembler = assembler t
          let le        = assembler.Lambda
          let ps        = le.Parameters |> Seq.toArray
          let nts       = t.FullName::ts
          let gps       = ps |> Array.map (fun p -> generator nts p.Type)
          let mi        = rFromLambda.MakeGenericMethod [|t|]
          mi.Invoke (null, [|le; gps|]) :?> Generator
      with
      | e ->
        raiseaf (Some e) "Could not create generator for type %s (%A)" t.FullName ts
      )

    and generator ts t = 
      match generators.TryGetValue t with
      | true  , generator -> generator  // This avoid unnecessary creation of createGenerator ts
      | false , _         -> generators.GetOrAdd (t, createGenerator ts)

    static member FromLambda<'T> (le : LambdaExpression) (gps : Generator []) =
      let parb        = Expression.Parameter (typeof<Arbitrary>       , "arb" )
      let psize       = Expression.Parameter (typeof<int>             , "size")
      let prg         = Expression.Parameter (typeof<RandomGenerator> , "rg"  )
      let vgrs        = gps |> Array.mapi (fun i gp -> Expression.Variable (rGeneratorResult gp.Generates, sprintf "gr%d" i))
      let rt          = rGeneratorResult le.ReturnType
      let rtc         = rt.GetConstructors().[0]  // TODO: 
      let e           = gps.Length - 1
      let breake      = Expression.Call (rBreak) :> Expression
      let expressions =
        [|
          for i = 0 to e do
            let rg = 
              if i = 0 then 
                prg :> Expression
              else 
                Expression.Property (vgrs.[i - 1], "RandomGenerator") :> Expression
            let g  = Expression.Constant (gps.[i], rGenerator gps.[i].Generates) :> Expression
            let mi = g.Type.GetMethod "Generate"  // TODO: 
            let ps = [|parb :> Expression; psize :> Expression; rg|]
//            yield breake
            yield Expression.Assign (vgrs.[i], Expression.Call (g, mi, ps)) :> Expression
          let rg = 
            if vgrs.Length = 0 then 
              prg :> Expression
            else 
              Expression.Property (vgrs.[e], "RandomGenerator") :> Expression
          let ve = Expression.Invoke(le, vgrs |> Array.map (fun vgr -> Expression.Property (vgr, "Value") :> Expression))
          yield Expression.New (rtc, rg, ve) :> Expression
        |]
      let block   = Expression.Block (
                        rt
                      , vgrs
                      , expressions
                      )
      let l       = Expression.Lambda<Func<Arbitrary, int, RandomGenerator, GeneratorResult<'T>>> (block, [|parb; psize; prg|])
      let f       = l.Compile ()
      { new Generator<'T>() with
        override x.Generate arb size rg = f.Invoke (arb, size, rg)
      }

    member x.Register (g : Generator    ) : unit = generators.[g.Generates]       <- g
    member x.Register (s : Shrinker     ) : unit = shrinkers.[s.Shrinks]          <- s
    member x.Register (a : Assembler    ) : unit = assemblers.[a.Assembles]       <- a
    member x.Register (d : Disassembler ) : unit = disassemblers.[d.Disassembles] <- d

    member x.Generator<'T> () = generator [] typeof<'T> :?> Generator<'T>

  end

and [<AbstractClass>] Generator<'T> () =
  class
    inherit Generator ()
    override x.Generates = typeof<'T>
    abstract Generate : Arbitrary -> int -> RandomGenerator -> GeneratorResult<'T>
  end

and [<AbstractClass>] Shrinker<'T> () =
  class 
    inherit Shrinker ()
    override x.Shrinks = typeof<'T>
    abstract Shrink : Arbitrary -> 'T -> Stream<'T>
  end

module StdGen =
  // Borrowed from FsCheck which borrowed from haskell

  module MagicConstants =
    let q1, q2    = 53668       , 52774
    let a1, a2    = 40014       , 40692
    let r1, r2    = 12211       , 3791
    let m1, m2    = 2147483563  , 2147483399
    
  open MagicConstants

  let create (seed : int64) : RandomGenerator = 
    let divMod64 (n : int64) (d : int64) : struct (int64*int64) = 
      let q = n / d
      let r = n % d
      if (Math.Sign (r) = -Math.Sign (d)) then struct (q - 1L, r + d) else struct (q, r)

    let mod64 (n : int64) (d : int64) : int64 = 
      let r = n % d
      if (Math.Sign (r) = -Math.Sign (d)) then r + d else r

    let seed            = abs seed
    let struct (q, s1)  = divMod64  seed  (int64 (m1 - 1))  //2147483562L
    let s2              = mod64     q     (int64 (m2 - 1)) //2147483398L
    RandomGenerator (int (s1 + 1L),int (s2 + 1L))

  let createFromCurrentTime () : RandomGenerator =  create DateTime.Now.Ticks

  let next (RandomGenerator (s1 ,s2)) : RandomGenerator =
    let k     = s1 / q1
    let s1'   = a1 * (s1 - k * q1) - k * r1
    let s1''  = if (s1' < 0) then s1 + m1 else s1'
    let k'    = s2 / q2
    let s2'   = a2 * (s2 - k' * q2) - k' * r2
    let s2''  = if s2' < 0 then s2' + m2 else s2'
    RandomGenerator (s1'', s2'')

  let split ((RandomGenerator (s1,s2)) as rg) : struct (RandomGenerator*RandomGenerator) = 
    let s1'   = if s1 = (m1 - 1) then 1 else s1 + 1
    let s2'   = if s2 = 1 then (m2 - 1) else s2 - 1
    let (RandomGenerator (ns1, ns2)) = next rg
    let left  = RandomGenerator (s1', ns2)
    let right = RandomGenerator (ns1, s2')
    struct (left,right)

  let sample (RandomGenerator (s1 ,s2)) : int =
    let z     = s1 - s2
    let z'    = if z < 1 then z + m1 - 1 else z
    z'

  let range (lo : int) (hi : int) (rg : RandomGenerator) : int =
    let lo = min lo hi
    let hi = max lo hi
    let r  = hi - lo + 1
    lo + (sample rg) % r  // TODO: This gives bias and generally reduces randomness

module Generator =
  let inline gresult rg v = GeneratorResult<_> (rg, v)

  // Monad

  let inline greturn v =
    { new Generator<_> () with
        override x.Generate arb size rg = gresult rg v
    }
    
  let inline gbind (uf : 'T -> Generator<'U>) (t : Generator<'T>) =
    { new Generator<_> () with
        override x.Generate arb size rg =
          let tr  = t.Generate arb size rg
          let u   = uf tr.Value
          u.Generate arb size tr.RandomGenerator
    }

  // Applicative

  let inline gpure f = greturn f

  let inline gapply (t : Generator<'T>) (f : Generator<'T -> 'U>) =
    { new Generator<_> () with
        override x.Generate arb size rg =
          let fr  = f.Generate arb size rg
          let tr  = t.Generate arb size fr.RandomGenerator
          gresult tr.RandomGenerator (fr.Value tr.Value)
    }

  // Functor

  let inline gmap (m : 'T -> 'U) (t : Generator<'T>) =
    { new Generator<_> () with
        override x.Generate arb size rg =
          let tr  = t.Generate arb size rg
          gresult tr.RandomGenerator (m tr.Value)
    }

  let gint : Generator<int> =
    { new Generator<_> () with
        override x.Generate arb size rg =
          let nrg = StdGen.next rg
          let v   = StdGen.range 0 size nrg
          gresult nrg v
    }
  
  let gstring : Generator<string> =
    { new Generator<_> () with
        override x.Generate arb size rg =
          let nrg = StdGen.next rg
          let v   = StdGen.range 0 size nrg |> string
          gresult nrg v
    }
  
  type Builder () =
    member inline x.Bind       (t, uf)   = gbind   uf t
    member inline x.Return     v         = greturn  v
    member inline x.ReturnFrom t         = t        : Generator<_>

open Generator

let generator = Builder ()

type Generator<'T> with
  static member inline (>>=) (x, uf) = gbind  uf x
  static member inline (|>>) (x, m)  = gmap    m x
  static member inline (<*>) (x, t)  = gapply  t x

let emptyArbritrary () = Arbitrary ()

let defaultArbritrary () = 
  let arb = Arbitrary ()
  arb.Register gint
  arb.Register gstring
  arb
  
