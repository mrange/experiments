namespace DrawInstanced

open System

open SharpDX
open SharpDX.Mathematics.Interop
open System.Diagnostics

module Common =

  let inline clamp v b e =
    if v < b    then b
    elif v > e  then e
    else v

  let pi            = float32 Math.PI
  let tau           = 2.F * pi

  let frameCount    = 2

  let transform (m : Matrix) (v : Vector3) : Vector3 =
    Vector3.TransformCoordinate(v, m)

  let newClock () =
    let f  = Stopwatch.Frequency |> float32
    let sw = Stopwatch ()
    sw.Start ()
    fun () ->
      float32 sw.ElapsedMilliseconds / 1000.F

  let dispose nm (it : IDisposable) =
    try
      if it <> null then it.Dispose ()
    with
    | e -> printfn "Failed to dispose %s" nm

  let rtrue         = RawBool true
  let rfalse        = RawBool false

  let inline (!?) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

  let rviewPortf  (v : Viewport)  : RawViewportF  = !? v
  let rrectangle  (r : Rectangle) : RawRectangle  = !? r
  let rcolor4     (c : Color4)    : RawColor4     = !? c

  let random        = Random 19740531

  let randomVector1 () = random.NextFloat (-1.F, 1.F)
  let randomVector3 () =
    let v = Vector3 ( randomVector1 ()
                    , randomVector1 ()
                    , randomVector1 ()
                    )

    v.Normalize ()

    v

  let normalize (v3 : Vector3) : Vector3 =
    v3.Normalize ()
    v3

 

  let lerp (s : float32) (f : Vector4) (t : Vector4) : Vector4 =
    f + (clamp s 0.F 1.F)*(t - f)
