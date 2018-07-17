// Raymarching
//  http://9bitscience.blogspot.com/2013/07/raymarching-distance-fields_14.html
open System
open System.Diagnostics
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Threading
open System.Collections.Concurrent
open System.Numerics

module Common = 

  let dispatch (d : Dispatcher) (a : unit -> unit) = 
    let a' = Action a
    ignore <| d.BeginInvoke (DispatcherPriority.ApplicationIdle, a')

  type V1                 = Single
  type V2                 = Vector2
  type V3                 = Vector3
  type V4                 = Vector4
 
  let inline v1 x         = float32 x
  let inline v2 x y       = Vector2 (v1 x, v1 y)
  let inline v3 x y z     = Vector3 (v1 x, v1 y, v1 z)
  let inline v4 x y z w   = Vector4 (v1 x, v1 y, v1 z, v1 w)

  let pi                  = v1 Math.PI
  let tau                 = 2.F * pi

  let inf                 = Single.PositiveInfinity
  let nan                 = Single.NaN

  let inline degree2rad d = pi * v1 d / 180.F
  let inline rad2degree r = v1 r * 180.F / pi

  let inline dot (l : ^V) (r : ^V) : V1 = 
    (^V  : (static member Dot : ^V  -> ^V  -> V1) (l, r))

  let inline length (v : ^V) : V1 = 
    (^V  : (member Length : unit -> V1) v)

  let inline normalize (v : ^V) : ^V = 
    (^V  : (static member Normalize : ^V  -> ^V) v)

  let inline cross (x : V3) (y : V3) : V3 =
    v3 (x.Y * y.Z - x.Z * y.Y) (x.Z * y.X - x.X * y.Z) (x.X * y.Y - x.Y * y.X)      

  let inline clamp x min max  = 
      if x < min then min
      elif x > max then max
      else x

  let inline norm x           = clamp x -1.F 1.F
  let inline unorm x          = clamp x 0.F 1.F

  let inline asByte d         = byte ((unorm d) * 255.F)

  let sequential (vs : seq<Async<_>>) : Async<_> =
    async {
      do! Async.SwitchToThreadPool ()
      return vs |> Seq.map (fun v -> Async.RunSynchronously v) |> Seq.toArray
    }

open Common

module Primitives =
  type Primitive = V3 -> struct (V1*Color)

  let sphere c r : Primitive =
    let r = v1 r
    fun v -> struct (length v - r, c)

  let plane c n o : Primitive =
    let n = normalize n
    let o = v1 o
    fun v -> struct (dot v n + o, c)

  let translate t (p : Primitive) : Primitive =
    fun v -> p (v - t)
    

type ViewPort = 
  {
    Eye             : V3
    At              : V3
    Up              : V3
    ClipDistance    : V1
    Fov             : V1
    Ratio           : V1
    Center          : V3
    Normal          : V3
    Axis0           : V3
    Axis1           : V3
    Width           : V1
    Height          : V1
    Corner0         : V3
    Corner1         : V3
    Corner2         : V3
    Corner3         : V3
  }

  static member New (eye : V3) (at : V3) (up : V3) (clipDistance : V1) (fov : V1) (ratio : V1) = 
    let clipNormal  = normalize (at - eye)
    let clipCenter  = eye + clipDistance * clipNormal

    let width       = clipDistance * tan (fov / 2.F)
    let height      = width * ratio

    let xaxis       = normalize (cross up clipNormal)
    let yaxis       = normalize (cross clipNormal xaxis)

    let halfx       = xaxis * (width / 2.F)
    let halfy       = yaxis * (height / 2.F)

    {
      Eye           = eye
      At            = at
      Up            = up
      ClipDistance  = clipDistance
      Fov           = fov
      Ratio         = ratio
      Center        = clipCenter
      Normal        = clipNormal
      Axis0         = xaxis
      Axis1         = yaxis
      Width         = width
      Height        = height
      Corner0       = clipCenter - halfx - halfy
      Corner1       = clipCenter + halfx - halfy
      Corner2       = clipCenter + halfx + halfy
      Corner3       = clipCenter - halfx + halfy
    }

module Raymarch =
  open Primitives

  let epsilon   = 0.1F

  let maxSteps  = 64

  let stepColors=
    [|
      for i = 0 to maxSteps - 1 do
        yield Color.FromRgb(255uy, 0uy, (255.F * v1 i / v1 maxSteps) |> round |> byte)
    |]

  let objects   =
    [|
        plane   Colors.Yellow (v3 0 1 0) 3
        sphere  Colors.Green  1
        sphere  Colors.Green  1 |> translate (v3 3 0 0)
    |]

  module Details =
    module Loops =
      let initial = struct (inf, Colors.Black)

      let rec closest current min i =
        if i < objects.Length then
          let t               = objects.[i] current
          let struct (dd, _)  = min
          let struct (d, _)   = t
          if d < dd then
            closest current t (i + 1)
          else
            closest current min (i + 1)
        else
          min

      let rec rayMarch current (direction : V3) minimumDistance i =
        if i < maxSteps then
          let struct (d, c) = closest current initial 0

          if d < epsilon then
            stepColors.[i]
            //c
          else
            let next = current + d * direction
            rayMarch next direction d (i + 1)
//          else
//            Colors.Black
        else
          Colors.Black
        
  open Details
  
  let rayMarch (viewPort : ViewPort) x y w h =
    let current   = viewPort.Eye
    let clip      = 
      viewPort.Corner0 
      + viewPort.Width * (v1 x / v1 w) * viewPort.Axis0 
      + viewPort.Height * (v1 y / v1 h) * viewPort.Axis1
    let direction = normalize (clip - current)

    Loops.rayMarch current direction inf 0

module View =

  let eye           = v3 0 1 -5
  let at            = v3 0 0 0
  let clipDistance  = 1.0F
  let clipUp        = v3 0 -1 0
  let fov           = degree2rad 120
  let granularity   = 1.F

  let viewPort r    = ViewPort.New eye at clipUp clipDistance fov r

  let openView () : unit =
    let window = new Window()
    window.MinWidth   <- 640.
    window.MinHeight  <- 400.

    use loaded = window.Loaded.Subscribe (fun v -> 
      let width   = v1 window.Width  / granularity
      let height  = v1 window.Height / granularity

      let wb      = new WriteableBitmap(int width, int height, 96., 96., PixelFormats.Bgr32, null)

      let iwidth  = wb.PixelWidth
      let iheight = wb.PixelHeight

      let viewPort= viewPort ((v1 iheight) / (v1 iwidth))

      let queue   = ConcurrentQueue<int*byte[]>()

      let eventHandler o e =
        let value = ref (0, [||])
        while queue.TryDequeue(value) do
          let x, pixels = !value
          ignore <|  wb.Lock()

          try 
            wb.WritePixels (Int32Rect (x, 0, 1, iheight), pixels, 4, 0)
          finally 
            wb.Unlock()

      let timer = 
        DispatcherTimer(
          TimeSpan.FromMilliseconds(200.)     ,
          DispatcherPriority.ApplicationIdle  ,
          EventHandler eventHandler           ,
          window.Dispatcher                   )

      timer.Start()

      let tracers = 
        seq {
          for x in 0..iwidth - 1 -> 
            async {
              let pixels = Array.zeroCreate (4 * iheight)

              for y in 0..iheight - 1 do
                let color = Raymarch.rayMarch viewPort x y iwidth iheight
//                let color = Raymarch.rayMarch viewPort (iwidth / 2) (iheight / 2) iwidth iheight
                pixels.[y*4 + 0] <- color.B
                pixels.[y*4 + 1] <- color.G
                pixels.[y*4 + 2] <- color.R
                pixels.[y*4 + 3] <- 255uy

              queue.Enqueue(x, pixels);
            }
        }

      let tracer = 
        tracers
        |> Async.Parallel 
//        |> sequential
        |> Async.Ignore

      let sw = Stopwatch ()

      let complete = 
        async {
          do! tracer

          dispatch window.Dispatcher (fun () -> 
            sw.Stop()
            window.Title <- sprintf "Trace took %f secs" sw.Elapsed.TotalSeconds
            )
        }

      sw.Start()

      Async.Start complete

      let i = new Image ()
      i.Source <- wb
      window.Content <- i
      )

    let result = window.ShowDialog()

    ()

open View

[<EntryPoint>]
[<STAThread>]
let main argv = 
  openView ()

  0


