
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
    
type TurtleMessage = 
    {
        Turtle  : unit -> List<Line>
        Reply   : List<Line> -> unit
    }
    static member New t r = {Turtle = t; Reply = r;}

let turtleWindow (turtleGenerator : float32 -> Turtle.Turtle<unit>) = 
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

    let turtleProcessor (ct : CancellationToken) (input : MailboxProcessor<TurtleMessage>) : Async<unit> =
        async {
            while not ct.IsCancellationRequested do
                let! message = input.Receive()
                let lines = message.Turtle ()
                message.Reply lines
        }

    let sw = Stopwatch()
    sw.Start()

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

    use mp = MailboxProcessor.Start (turtleProcessor ct, ct)

    use onExitCancelTask    = OnExit cts.Cancel

    let resizer             = EventHandler(fun o e -> recreateDevice ())

    form.Resize.AddHandler  resizer

    use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

    Windows.RenderLoop.Run(form, fun () -> 

        let turtle = turtleGenerator <| float32 sw.Elapsed.TotalSeconds
        mp.Post <| TurtleMessage.New (fun () -> turtleExecutor turtle) (fun lines -> currentLines := lines)
            
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


[<STAThread>]
[<EntryPoint>]
let main argv = 

    turtleWindow <| TreeFractal.Generate 10 250.F


    0
