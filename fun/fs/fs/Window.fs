namespace FolderSize

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic

open SharpDX

module Window = 

    let Show (source : IObservableSource) (input : IObservable<VisualTree>) = 

        use form                = new Windows.RenderForm("FolderSize")

        form.ClientSize         <- System.Drawing.Size(1600,1200)

        let device              = ref <| new Device(form)

        let disposeDevice ()    = TryDispose !device
        let recreateDevice ()   = disposeDevice ()
                                  device := new Device(form)

        use onExitDisposeDevice = OnExit disposeDevice

        use cts = new CancellationTokenSource ()
        let ct = cts.Token

        use onExitCancelTask    = OnExit cts.Cancel

        let resizer             = EventHandler(fun o e -> recreateDevice ())

        form.Resize.AddHandler  resizer

        use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

        let vt = ref VisualTree.NoVisual

        use terminator = input |> ObservableEx.terminator_Next (fun v -> vt := v)

        source.Start ()

        Windows.RenderLoop.Run(form, fun () -> 

            let d = !device

            d.Draw <| fun d2dRenderTarget -> 
                
                d2dRenderTarget.Clear(AsNullable <| Color.White.ToColor4())

                let tfc        = d.GetTextFormat
                let bc (bd, o) = let b = d.GetBrush bd
                                 if b <> null then b.Opacity <- o
                                 b

                let transform = Animated.Constant <| Matrix3x2.Scaling d.Height

                let appState = CurrentState ()

                Visual.RenderTree appState d2dRenderTarget tfc bc <| Transform (transform,!vt)
                )
