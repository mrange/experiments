namespace FolderSize

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic

open SharpDX

module Window = 

    let Show () = 
        let sw = Stopwatch()
        sw.Start()

        use form                = new Windows.RenderForm("FolderSize")

        form.ClientSize         <- System.Drawing.Size(1600,1200)

        let device              = ref <| new Device(form)

        let disposeDevice ()    = TryRun (upcast !device : IDisposable).Dispose
        let recreateDevice ()   = disposeDevice ()
                                  device := new Device(form)

        use onExitDisposeDevice = OnExit disposeDevice

        use cts = new CancellationTokenSource ()
        let ct = cts.Token

        use onExitCancelTask    = OnExit cts.Cancel

        let resizer             = EventHandler(fun o e -> recreateDevice ())

        form.Resize.AddHandler  resizer

        use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

        Windows.RenderLoop.Run(form, fun () -> 

            let d = !device

            d.Draw <| fun d2dRenderTarget -> 
                
                d2dRenderTarget.Clear(Nullable<_>(Color.White.ToColor4()))

                )
