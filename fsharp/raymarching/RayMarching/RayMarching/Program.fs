open System
open System.Diagnostics
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Threading
open System.Collections.Concurrent

[<EntryPoint>]
[<STAThread>]
let main argv = 
  let dispatch (d : Dispatcher) (a : unit -> unit) = 
    let a' = Action a
    ignore <| d.BeginInvoke (DispatcherPriority.ApplicationIdle, a')

  let window        = new Window()
  window.MinWidth   <- 640.
  window.MinHeight  <- 400.

  let granularity   = 1

  use loaded        = window.Loaded.Subscribe (fun v -> 
        

    let width   = window.Width  / (float granularity)
    let height  = window.Height / (float granularity)
    let ratio   = width / height

    let wb      = new WriteableBitmap(int width, int height, 96., 96., PixelFormats.Bgr32, null)

    let iwidth  = wb.PixelWidth
    let iheight = wb.PixelHeight

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
      |>  Async.Parallel 
      |>  Async.Ignore

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

  0


