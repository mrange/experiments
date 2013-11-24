
open System
open System.Diagnostics

open SharpDX
open TurtlePower

[<STAThread>]
[<EntryPoint>]
let main argv = 
    use form                = new Windows.RenderForm("Turtle Power")

    let device              = ref <| new Device(form)

    let disposeDevice ()    = TryRun (!device :> IDisposable).Dispose
    let recreateDevice ()   = disposeDevice ()
                              device := new Device(form)

    try 
        form.Resize.Add         <|fun v -> recreateDevice ()
                                    

        let sw = Stopwatch()
        sw.Start()

        let turtleGenerator = TreeFractal.Generate 11 250.F
    
        Windows.RenderLoop.Run(form, fun () -> 
            
            let ddr = !device
            let colors              =   [
                                            Turtle.Brown        , ddr.BrownBrush
                                            Turtle.LimeGreen    , ddr.LimeGreenBrush
                                            Turtle.Lime         , ddr.LimeBrush
                                        ] 
                                        |> List.fold (fun s (c,b) -> s |> Map.add c b) Map.empty
                                

            let turtle = turtleGenerator <| float32 sw.Elapsed.TotalSeconds
            ddr.Draw (fun d2dRenderTarget -> 
                let transform = 
                    Matrix3x2.Identity 
                    <*> Matrix3x2.Rotation (Deg2Rad * 180.F)
                    <*> Matrix3x2.Translation (ddr.Width/2.F, ddr.Height - 40.F) 
                d2dRenderTarget.Transform <- transform

                let executor =  Turtle.Execute 
                                    Turtle.Brown
                                    3.F 
                                    (NewVector2 0.F 0.F) 
                                    (NewVector2 0.F 1.F)
                                    (fun c w f t -> d2dRenderTarget.DrawLine(f, t, colors.[c], w))

                d2dRenderTarget.Clear(Nullable<_>(Color.White.ToColor4()))

                ignore <| executor turtle
                )
            )
    finally
        TryRun disposeDevice
    0
