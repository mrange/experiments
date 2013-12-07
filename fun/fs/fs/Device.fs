namespace FolderSize

open System
open System.Diagnostics

open SharpDX
open SharpDX

type ColorDescriptor = 
    {
        Alpha   : float32
        Red     : float32
        Green   : float32
        Blue    : float32
    }
    static member ARGB  a r g b     =   {Alpha = a; Red = r; Green = g; Blue = b}
    static member RGB   r g b       =   ColorDescriptor.ARGB 1.F r g b
    static member Color (c : Color) =   let toFloat (b : byte) = (float32 b) / 255.0F
                                        ColorDescriptor.ARGB (toFloat c.A) (toFloat c.R) (toFloat c.G) (toFloat c.B)
    member x.ToColor4               =   Color4(x.Red, x.Green, x.Blue, x.Alpha)
    member x.ToColor3               =   Color3(x.Red, x.Green, x.Blue)

type BrushDescriptor    =
    |   Transparent
    |   SolidColor  of ColorDescriptor

type TextFormatDescriptor   =
    {
        FontFamily  : string
        FontSize    : float32
    }
    static member New ff fs = {FontFamily = ff; FontSize = fs}

type Device(form : Windows.RenderForm) = 

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
        desc.SwapEffect         <- DXGI.SwapEffect.Sequential
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

    let width               = float32 form.ClientSize.Width
    let height              = float32 form.ClientSize.Height

    let dwFactory           = new DirectWrite.Factory (DirectWrite.FactoryType.Isolated)
    let d2dFactory          = new Direct2D1.Factory(Direct2D1.FactoryType.SingleThreaded)
    let device, swapChain   = GetDeviceAndSwapChain form
    let factory             = swapChain.GetParent<DXGI.Factory>()

    let associateWithWindow = factory.MakeWindowAssociation(form.Handle, DXGI.WindowAssociationFlags.IgnoreAll)

    let backBuffer          = Direct3D11.Texture2D.FromSwapChain<Direct3D11.Texture2D>(swapChain, 0)
    let surface             = backBuffer.QueryInterface<SharpDX.DXGI.Surface>();
    let d2dRenderTarget     = new Direct2D1.RenderTarget(
                                d2dFactory                          , 
                                surface                             , 
                                Direct2D1.RenderTargetProperties(
                                    Direct2D1.PixelFormat(
                                        DXGI.Format.Unknown         , 
                                        Direct2D1.AlphaMode.Premultiplied
                                        )
                                    )
                                )

    let mutable brushCache : Map<BrushDescriptor, Direct2D1.Brush> = Map.empty

    let mutable textFormatCache : Map<TextFormatDescriptor, DirectWrite.TextFormat> = Map.empty

    let solid (c : ColorDescriptor)   = 
        new Direct2D1.SolidColorBrush(d2dRenderTarget, c.ToColor4)                                    

    member x.GetBrush (bd : BrushDescriptor) : Direct2D1.Brush =
        let find = brushCache.TryFind bd
        match find with
        | Some b    -> b
        | _         ->
            let b : Direct2D1.Brush= 
                match bd with
                | Transparent       -> null
                | SolidColor c      -> upcast solid c
            brushCache <- brushCache |> Map.add bd b
            b
        
    member x.GetTextFormat (tfd : TextFormatDescriptor) : DirectWrite.TextFormat = 
        let find = textFormatCache.TryFind tfd
        match find with
        | Some tf   -> tf
        | _         ->
            let tf = new DirectWrite.TextFormat(dwFactory, tfd.FontFamily, tfd.FontSize)
            textFormatCache <- textFormatCache |> Map.add tfd tf
            tf
        


    member x.Width              = width
    member x.Height             = height

    member x.Draw (a : Direct2D1.RenderTarget->unit) =
        d2dRenderTarget.BeginDraw()
        try
            a d2dRenderTarget
        finally
            d2dRenderTarget.EndDraw()
            swapChain.Present (1, DXGI.PresentFlags.None)


    interface IDisposable with
        member x.Dispose() =
            let tfc = textFormatCache
            textFormatCache <- Map.empty
            for kv in tfc do
                TryDispose kv.Value 
            let bc = brushCache
            brushCache <- Map.empty
            for kv in bc do
                TryDispose kv.Value 
            TryDispose d2dRenderTarget
            TryDispose surface
            TryDispose backBuffer
            TryDispose factory
            TryDispose swapChain
            TryDispose device
            TryDispose d2dFactory
            TryDispose dwFactory
            



