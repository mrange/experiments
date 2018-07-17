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

  let inline degree2rad d = pi * v1 d / 180.F
  let inline rad2degree r = v1 r * 180.F / pi

(*
  let inline dot (l : ^V when ^V : (static member Dot : ^V  -> ^V  -> ^V)) (r : ^V) : ^V = 
    (^V  : (static member Dot : ^V  -> ^V  -> ^V) l r)
*)

  let inline length (v : ^V when ^V : (member Length : V1)) : V1 = 
    (^V  : (member Length : V1) v)

  let inline normalize (v : ^V when ^V : (static member Normalize : ^V  -> ^V)) : ^V = 
    (^V  : (static member Normalize : ^V  -> ^V) v)

  let inline cross (x : V3) (y : V3) : V3 =
    v3 (x.Y * y.Z - x.Z * y.Y) (x.Z * y.X - x.X * y.Z) (x.X * y.Y - x.Y * y.X)      

open Common

type ViewPort = 
  {
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

  static member New (eye :V3) (at : V3) (up : V3) (clipDistance : V1) (fov : V1) (ratio : V1) = 
    let clipNormal  = normalize (at - eye)
    let clipCenter  = eye + clipDistance * clipNormal

    let width       = clipDistance * tan (fov / 2.F)
    let height      = width * ratio

    let xaxis       = normalize (cross up clipNormal)
    let yaxis       = normalize (cross clipNormal xaxis)

    let halfx       = xaxis * (width / 2.F)
    let halfy       = yaxis * (height / 2.F)

    {
      Center  = clipCenter
      Normal  = clipNormal
      Axis0   = xaxis
      Axis1   = yaxis
      Width   = width
      Height  = height
      Corner0 = clipCenter - halfx - halfy
      Corner1 = clipCenter + halfx - halfy
      Corner2 = clipCenter + halfx + halfy
      Corner3 = clipCenter - halfx + halfy
    }

module View =

  let eye           = v3 3 3 3
  let at            = v3 0 1 0
  let clipDistance  = 1.F
  let clipUp        = v3 0 1 0
  let fov           = degree2rad 120
  let granularity   = 1.F

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

      let viewPort = ViewPort.New eye at clipUp clipDistance fov ((v1 iheight) / (v1 iwidth))

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
              let pixels = 
                [| for i in 0..iheight - 1 do
                    yield byte 255
                    yield byte 0
                    yield byte 0
                    yield byte 255
                |]

              queue.Enqueue(x, pixels);
            }
        }

      let tracer = 
        tracers
        |> Async.Parallel 
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


