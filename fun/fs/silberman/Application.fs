namespace silberman

open SharpDX

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

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
    
    type private ToUIMessage =
        | NewVisual of VisualTree
        | ShutDownUI  

    type private FromUIMessage =
        | Exception of exn
        | ShutDownApplication  

    let private ShowForm 
        (title  : string                                    ) 
        (width  : int                                       )
        (height : int                                       ) 
        (ct     : CancellationToken                         )
        (toui   : ConcurrentQueue<ToUIMessage>              )
        (fromui : BlockingCollection<FromUIMessage>         )
        =

        use onExitShutDown      = OnExit <| fun () -> ignore (fromui.TryAdd ShutDownApplication); fromui.CompleteAdding()

        use form                = new Windows.RenderForm(title)

        form.ClientSize         <- System.Drawing.Size(width,height)

        use directWrite         = new DirectWrite()
        let device              = ref <| new WindowedDevice(form)

        let disposeDevice ()    = TryDispose !device
        let recreateDevice ()   = disposeDevice ()
                                  device := new WindowedDevice(form)

        use onExitDisposeDevice = OnExit disposeDevice

        let resizer             = EventHandler(fun o e -> recreateDevice ())

        form.Resize.AddHandler  resizer

        use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

        let context = Logical.Foundation.ElementContext.New directWrite.EstimateTextSize

        let document = Logical.Standard.DocumentElement(context)

        try 
            Windows.RenderLoop.Run(form, fun () -> 
                let vt = ref VisualTree.NoVisual

                let d = !device

                d.Draw <| fun d2dRenderTarget -> 
                
                    d2dRenderTarget.Clear(AsNullable <| Color.White.ToColor4())

                    let tfc        = d.DirectWrite.GetTextFormat
                    let bc (bd, o) = let b = d.GetBrush bd
                                     if b <> null then b.Opacity <- o
                                     b

                    let transform = Animated.Constant <| Matrix3x2.Scaling d.Height

                    let appState = CurrentState ()

                    Visual.RenderTree appState d2dRenderTarget tfc bc <| Transform (transform,!vt)

                if ct.IsCancellationRequested then () // TODO:

                let msg = RefOf<ToUIMessage>
                while toui.TryDequeue msg do
                    match !msg with
                    | ShutDownUI    -> ()   // TODO:
                    | NewVisual nvt -> vt := nvt

                )

            
        with
            | e -> ignore <| fromui.TryAdd (Exception e)

    let Show 
        (title  : string) 
        (width  : int   )
        (height : int   )
        (body   : Logical.Foundation.Element) = 
        
        let formProcessor ct toui fromui = async {
            do! Async.SwitchToNewThread ()

            let thread = Thread.CurrentThread

            thread.Priority <- ThreadPriority.AboveNormal

            ShowForm title width height ct toui fromui
            return ()
            }

        async {
            use fromui  = new BlockingCollection<FromUIMessage>() 
            let toui    = ConcurrentQueue<ToUIMessage>() 

            let! ct = Async.CancellationToken

            Async.StartImmediate <| formProcessor ct toui fromui

            let nextRebuild = ref <| CurrentTime () + 0.1F

            let cont = ref true

            try 

            while !cont && not <| ct.IsCancellationRequested do
                
                let waitFor = int <| 100.F *  max 0.F (!nextRebuild - CurrentTime ()) + 0.5F

                let! fromMessage = fromui.AsyncTryGet waitFor ct

                match fromMessage with
                | Some fromMessage ->
                    match fromMessage with
                    | ShutDownApplication   -> cont := false
                    | _                     -> ()
                | None ->
                    nextRebuild := CurrentTime () + 0.1F
                    ()

            finally 
                toui.Enqueue <| ShutDownUI

            return ()            
        }
        



