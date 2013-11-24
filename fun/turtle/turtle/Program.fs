
open System
open System.Diagnostics

open SharpDX
open TurtlePower

let GetDeviceAndSwapChain (form : Windows.RenderForm) =
    let width               = form.ClientSize.Width
    let height              = form.ClientSize.Height

    let mutable desc        = DXGI.SwapChainDescription()
    desc.BufferCount        <- 2
    desc.ModeDescription    <- SharpDX.DXGI.ModeDescription(
                                width                           ,
                                height                          ,
                                DXGI.Rational(60, 1)            ,
                                DXGI.Format.R8G8B8A8_UNorm
                                )
    desc.IsWindowed         <- Bool true
    desc.OutputHandle       <- form.Handle
    desc.SampleDescription  <- DXGI.SampleDescription(1,0)
    desc.SwapEffect         <- DXGI.SwapEffect.Discard
    desc.Usage              <- DXGI.Usage.RenderTargetOutput

    let device              = ref DefaultOf<Direct3D11.Device>
    let swapChain           = ref DefaultOf<DXGI.SwapChain>

    let featureLevels       = 
        [|
            Direct3D.FeatureLevel.Level_11_0
            Direct3D.FeatureLevel.Level_10_1
            Direct3D.FeatureLevel.Level_10_0
            Direct3D.FeatureLevel.Level_9_3
            Direct3D.FeatureLevel.Level_9_2
            Direct3D.FeatureLevel.Level_9_1
        |]

    Direct3D11.Device.CreateWithSwapChain(
        Direct3D.DriverType.Hardware                , 
        Direct3D11.DeviceCreationFlags.BgraSupport  , 
        featureLevels                               , 
        desc                                        , 
        device                                      , swapChain
        )

    !device, !swapChain


[<STAThread>]
[<EntryPoint>]
let main argv = 
    use form                = new Windows.RenderForm("Turtle Power")

    let width               = float32 form.ClientSize.Width
    let height              = float32 form.ClientSize.Height

    let d,sc                = GetDeviceAndSwapChain form

    use device              = d
    use swapChain           = sc

    use d2dFactory          = new Direct2D1.Factory(Direct2D1.FactoryType.SingleThreaded)
    use factory             = swapChain.GetParent<DXGI.Factory>()

    factory.MakeWindowAssociation(form.Handle, DXGI.WindowAssociationFlags.IgnoreAll)

    use backBuffer          = Direct3D11.Texture2D.FromSwapChain<Direct3D11.Texture2D>(swapChain, 0)
    use surface             = backBuffer.QueryInterface<SharpDX.DXGI.Surface>();
    use d2dRenderTarget     = new Direct2D1.RenderTarget(
                                d2dFactory                          , 
                                surface                             , 
                                Direct2D1.RenderTargetProperties(
                                    Direct2D1.PixelFormat(
                                        DXGI.Format.Unknown         , 
                                        Direct2D1.AlphaMode.Premultiplied
                                        )
                                    )
                                )

    use solidColorBrush     = new Direct2D1.SolidColorBrush(d2dRenderTarget, Color.Brown.ToColor4())

    let sw = Stopwatch()
    sw.Start()

    let turtleGenerator = TreeFractal.Generate 9 200.F (sqrt 2.F) 
    
    Windows.RenderLoop.Run(form, fun () -> 
        
        let degree = float32 <| (sw.Elapsed.TotalSeconds * 10.) % 180.
        let turtle = turtleGenerator degree
        d2dRenderTarget.BeginDraw()
        try
            let transform = 
                Matrix3x2.Identity 
                <*> Matrix3x2.Rotation (Deg2Rad * 180.F)
                <*> Matrix3x2.Translation (width/2.F, height) 
            d2dRenderTarget.Transform <- transform

            let executor =  Turtle.Execute 
                                3.F 
                                (NewVector2 0.F 0.F) 
                                (NewVector2 0.F 1.F)
                                (fun w f t -> d2dRenderTarget.DrawLine(f, t, solidColorBrush, w))

            d2dRenderTarget.Clear(Nullable<_>(Color.White.ToColor4()))

            ignore <| executor turtle

        finally
            d2dRenderTarget.EndDraw()
            swapChain.Present(0, DXGI.PresentFlags.None)
        )

    0
