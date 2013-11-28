
open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic

open SharpDX
open TurtlePower

type Line = 
    {
        Color   : Turtle.Color
        Width   : float32
        From    : Vector2
        To      : Vector2
    }
    static member New c w f t = {Color = c; Width = w; From = f; To = t}
    

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let turtleGenerator = TreeFractal.Generate 10 250.F

    let turtleExecutor (t : Turtle.Turtle<unit>) : List<Line> =
        let lines = List<Line>(64)
        ignore <| Turtle.Execute 
            Turtle.Brown
            3.F 
            (NewVector2 0.F 0.F) 
            (NewVector2 0.F 1.F)
            (fun c w f t -> lines.Add <| Line.New c w f t)
            t
        lines

    use form                = new Windows.RenderForm("Turtle Power")

    form.ClientSize         <- System.Drawing.Size(1600,1200)

    let device              = ref <| new Device(form)

    let disposeDevice ()    = TryRun (!device :> IDisposable).Dispose
    let recreateDevice ()   = disposeDevice ()
                              device := new Device(form)

    use onExitDisposeDevice = OnExit disposeDevice

    use cts = new CancellationTokenSource()
    let ct = cts.Token

    let turtle = turtleGenerator 0.F
    let currentLines = ref <| turtleExecutor turtle

    let turtleCreator = 
        async {
            Thread.CurrentThread.Priority <- ThreadPriority.Lowest

            let sw = Stopwatch()
            sw.Start()

            while not ct.IsCancellationRequested do
                let turtle = turtleGenerator <| float32 sw.Elapsed.TotalSeconds
                currentLines := turtleExecutor turtle
        }

    use task                = Async.StartAsTask turtleCreator

    use onExitWaitForTask   = OnExit task.Wait
    use onExitCancelTask    = OnExit cts.Cancel

    form.Resize.Add         <|fun v -> recreateDevice ()

    Windows.RenderLoop.Run(form, fun () -> 
            
        let ddr = !device
        let colors              =   [
                                        Turtle.Brown            , ddr.BrownBrush
                                        Turtle.LimeGreen        , ddr.LimeGreenBrush
                                        Turtle.Lime             , ddr.LimeGreenBrush
                                        Turtle.MediumVioletRed  , ddr.MediumVioletRedBrush
                                    ] 
                                    |> List.fold (fun s (c,b) -> s |> Map.add c b) Map.empty
                                
        let lines = !currentLines

        ddr.Draw (fun d2dRenderTarget -> 
                
            d2dRenderTarget.Clear(Nullable<_>(Color.White.ToColor4()))

            let transform = 
                Matrix3x2.Identity 
                <*> Matrix3x2.Rotation (Deg2Rad * 180.F)
                <*> Matrix3x2.Translation (ddr.Width/2.F, ddr.Height - 20.F) 
            d2dRenderTarget.Transform <- transform

            for l in lines do
                d2dRenderTarget.DrawLine(l.From, l.To, colors.[l.Color], l.Width)
            )
        )

    0
