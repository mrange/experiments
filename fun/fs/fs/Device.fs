﻿namespace FolderSize

open System
open System.Diagnostics

open SharpDX
open SharpDX.Direct2D1

type ColorDescriptor = 
    {
        Alpha   : float32
        Red     : float32
        Green   : float32
        Blue    : float32
    }

type BrushDescriptor    =
    |   Transparent
    |   SolidColor  of ColorDescriptor

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

    let solid (c : ColorDescriptor)   = 
        let cc = new Color4(c.Red, c.Green, c.Blue, c.Alpha)
        new Direct2D1.SolidColorBrush(d2dRenderTarget, cc)                                    

    let mutable brushCache : Map<BrushDescriptor, Brush> = Map.empty

    member x.GetBrush (bd : BrushDescriptor) : Brush =
        let find = brushCache.TryFind bd
        match find with
        | Some b    -> b
        | _         ->
            let b : Brush= 
                match bd with
                | Transparent       -> null
                | SolidColor c      -> upcast solid c
            brushCache <- brushCache |> Map.add bd b
            b
        

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
            



