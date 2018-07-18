// Raymarching
//  http://9bitscience.blogspot.com/2013/07/raymarching-distance-fields_14.html
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Threading
open System.Windows.Media.Imaging
open System.Collections.Concurrent
open System.Diagnostics
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

  let v3zero              = V3()

  let inline clamp x min max  = 
    if x < min then min
    elif x > max then max
    else x

  let inline norm x           = clamp x -1.F 1.F
  let inline unorm x          = clamp x 0.F 1.F

  let inline asByte d         = byte ((unorm d) * 255.F)

  let inline amod x y         = abs x % abs y
  let inline (%%) x y         = amod x y

  let inline vdot (x : ^V) (y : ^V) : V1 = 
    (^V  : (static member Dot : ^V -> ^V -> V1) (x, y))

  let inline vl2 (x : ^V) : V1 = 
    (^V  : (member Length : unit -> V1) x)

  let inline vlerp (x : ^V) (y : ^V) (f : V1) : ^V = 
    (^V  : (static member Lerp : ^V -> ^V -> V1 -> ^V) (x, y ,f))

  let inline vnormalize (x : ^V) : ^V = 
    (^V  : (static member Normalize : ^V -> ^V) x)

  let inline vabs (x : ^V) : ^V =
    (^V  : (static member Abs : ^V -> ^V) x)

  let inline vmin (x : ^V) (y : ^V) : ^V =
    (^V  : (static member Min : ^V -> ^V -> ^V) (x, y))

  let inline vmax (x : ^V) (y : ^V) : ^V =
    (^V  : (static member Max : ^V -> ^V -> ^V) (x, y))

  let inline vcross (x : V3) (y : V3) : V3 =
    (^V  : (static member Cross : ^V -> ^V -> ^V) (x, y))

  let inline vtrace (x : V3) (y : V3) (f : V1) : V3 = 
    x + f * y 

  let inline vmod (x : V3) (y : V3) : V3 = 
    v3 (x.X %% y.X) (x.Y %% y.Y) (x.Z %% y.Z)

  let sequential (vs : seq<Async<_>>) : Async<_> =
    async {
      do! Async.SwitchToThreadPool ()
      return vs |> Seq.map (fun v -> Async.RunSynchronously v) |> Seq.toArray
    }

open Common

module Colors =
  let black = V3 ()
  let white = v3 1 1 1
  let red   = v3 1 0 0
  let green = v3 0 1 0
  let blue  = v3 0 0 1
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
    let clipNormal  = vnormalize (at - eye)
    let clipCenter  = eye + clipDistance * clipNormal

    let width       = clipDistance * tan (fov / 2.F)
    let height      = width * ratio

    let xaxis       = vnormalize (vcross up clipNormal)
    let yaxis       = vnormalize (vcross clipNormal xaxis)

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

module Scenes =
  type Scene = V3 -> struct (V1*V3)

  let sphere c r : Scene =
    let r = v1 r
    fun v -> struct (vl2 v - r, c)

  let box c b : Scene =
    fun v ->
      let d = vabs v - b
      struct ((min (max d.X (max d.Y d.Z)) 0.F) + vl2 (vmax d v3zero), c)

  let plane c (n : V3) o : Scene =
    let n = vnormalize n
    let o = v1 o
    fun v -> struct (vdot v n + o, c)

  let union x y : Scene =
    fun v ->
      let struct (xd, xc) = x v
      let struct (yd, yc) = y v
      if yd < xd then
        struct (yd, yc)
      else
        struct (xd, xc)

  let intersect x y : Scene =
    fun v ->
      let struct (xd, xc) = x v
      let struct (yd, yc) = y v
      if yd > xd then
        struct (yd, yc)
      else
        struct (xd, xc)

  let subtract x y : Scene =
    fun v ->
      let struct (xd, xc) = x v
      let struct (yd, yc) = y v
      if yd > -xd then
        struct (yd, yc)
      else
        struct (-xd, yc)

  let repeat off s : Scene =
    let hoff = 0.5F * (off : V3)
    fun v ->
      let vv = vmod v off - hoff
      s vv

  let translate t (s : Scene) : Scene =
    fun v -> s (v - t)

  let objects =
    let boxes   = box Colors.blue (v3 0.1 0.1 0.1) |> repeat (v3 0.3 0.3 0.3)
    let sphere  = sphere Colors.red 2
    [|
      sphere |> subtract boxes
    |]

  module Details =
    module Loops =
      let rec scene current candidate i =
        if i < objects.Length then
          let t               = objects.[i] current
          let struct (td, _)  = t
          let struct (cd, _)  = candidate
          if td < cd then
            scene current t (i + 1)
          else
            scene current candidate (i + 1)
        else
          candidate

  open Details

  let scene current =
    let initial = struct (inf, Colors.black)
    Loops.scene current initial 0


module Renderer =
  let epsilon   = 0.00001F

  let maxSteps  = 128

  let lightPos  = v3 3 3 6
  let lightColor= Colors.white
  let ambient   = v3 0.4 0.4 0.4
  let sky       = v3 0.1 0.1 0.1

  let stepColors=
    [|
      for i = 0 to maxSteps - 1 do
        let s = v1 i / v1 maxSteps
        yield v3 1.F s s
    |]

  module Details =
    let normal scene current =
      let d = 0.0001F

      let struct (nxh, _) = scene (current + v3 +d 0 0)
      let struct (nxl, _) = scene (current + v3 -d 0 0)

      let struct (nyh, _) = scene (current + v3 0 +d 0)
      let struct (nyl, _) = scene (current + v3 0 -d 0)

      let struct (nzh, _) = scene (current + v3 0 0 +d)
      let struct (nzl, _) = scene (current + v3 0 0 -d)

      vnormalize (v3 (nxh - nxl) (nyh - nyl) (nzh - nzl))

    module Loops =
      let rec visibility_ scene p0 p1 r k maxt f t =
        if t < maxt then
          let struct (d,_ ) = scene (vtrace p0 r f)

          if (d < epsilon) then
            0.F
          else
            visibility_ scene p0 p1 r k maxt (min f (k*d/t)) (t + d)
        else
          f

      let visibility scene p0 p1 k =
        let r    = vnormalize (p1 - p0)
        let maxt = vl2 (p1 - p0)
        visibility_ scene p0 p1 r k maxt 1.F (10.F * epsilon)

      let shading scene current normal = 
        let v = visibility scene current lightPos 16.F
        if v > 0.F then
          let d = vnormalize (lightPos - current)
          let i = clamp (vdot normal d) 0.F 1.F
          lightColor*i + ambient*(1.F - i)
        else
          ambient

      let rec rayMarch scene origin direction t i =
        if i < maxSteps then
          let current = vtrace origin direction t
          let struct (d, c) = scene current

          if d < epsilon then
            let n = normal scene current
            let s = shading scene current n
            c*s
            //stepColors.[i]
          else
            rayMarch scene origin direction (t + d) (i + 1)
        else
          sky
        
  open Details
  
  let rayMarch (viewPort : ViewPort) (scene : Scenes.Scene) x y w h : V3 =
    let current   = viewPort.Eye
    let clip      = 
      viewPort.Corner0 
      + viewPort.Width * (v1 x / v1 w) * viewPort.Axis0 
      + viewPort.Height * (v1 y / v1 h) * viewPort.Axis1
    let direction = vnormalize (clip - current)

    Loops.rayMarch scene current direction 0.F 0

module View =

  let eye           = v3 1 3 3
  let at            = v3 0 0 0
  let clipDistance  = 1.0F
  let clipUp        = v3 0 -1 0
  let fov           = degree2rad 120
  let granularity   = 4.F

  let viewPort r    = ViewPort.New eye at clipUp clipDistance fov r

  let openView () : unit =
    let window = new Window ()
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
                let color = Renderer.rayMarch viewPort Scenes.scene x y iwidth iheight
//                let color = Raymarch.rayMarch viewPort (iwidth / 2) (iheight / 2) iwidth iheight
                pixels.[y*4 + 0] <- asByte color.Z
                pixels.[y*4 + 1] <- asByte color.Y
                pixels.[y*4 + 2] <- asByte color.X
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


