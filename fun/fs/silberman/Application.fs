namespace silberman

open SharpDX

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic

open Fundamental
open Device
open Visual
open Logical

module public Application =

    let GlobalClock =   let sw = new Stopwatch ()
                        sw.Start ()
                        sw

    let CurrentTime () : Time = 
        (float32 GlobalClock.ElapsedMilliseconds) / 1000.F


    let CurrentState () : ApplicationState = 
        ApplicationState.New (CurrentTime ()) (MouseState.New Set.empty (Vector2 ()))
    

    let Show 
        (title  : string) 
        (width  : int   )
        (height : int   )
        (body   : Logical.Foundation.Element) = 

        use form                = new Windows.RenderForm(title)

        form.ClientSize         <- System.Drawing.Size(width,height)

        let device              = ref <| new WindowedDevice(form)

        let disposeDevice ()    = TryDispose !device
        let recreateDevice ()   = disposeDevice ()
                                  device := new WindowedDevice(form)

        use onExitDisposeDevice = OnExit disposeDevice

        use cts = new CancellationTokenSource ()
        let ct = cts.Token

        use onExitCancelTask    = OnExit cts.Cancel

        let resizer             = EventHandler(fun o e -> recreateDevice ())

        form.Resize.AddHandler  resizer

        use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

        let vt = ref VisualTree.NoVisual

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
