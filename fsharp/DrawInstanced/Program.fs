open System
open System.Windows.Forms
open SharpDX

open DrawInstanced

open Common

[<EntryPoint>]
[<STAThread>]
let main argv =
  try
    Application.EnableVisualStyles ()
    Application.SetCompatibleTextRenderingDefault false

    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

#if DEBUG
    // Enable the D3D12 debug layer.
    Direct3D12.DebugInterface.Get().EnableDebugLayer ()
#endif

    use rf  = new Windows.RenderForm (Width = 3600, Height = 2000, StartPosition = FormStartPosition.CenterScreen)
    rf.AutoScaleMode <- AutoScaleMode.None

    use app = new App<_, _, _> (rf, LorenzAttractor.deviceIndependent, LorenzAttractor.deviceDependent)

    rf.Show ()

    app.Initialize ()

    use rl = new Windows.RenderLoop (rf)

    let clock = newClock ()

    let rec loop () =
      if rl.NextFrame () then
        app.Update (clock ())
        app.Render ()
        loop ()

    loop ()

    0
  with
  | e ->
    printfn "Exception: %s" e.Message
    999
