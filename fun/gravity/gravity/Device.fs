// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
// If you cannot locate the  Microsoft Public License, please send an email to
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

namespace GravitySucks

open System
open System.Diagnostics

open SharpDX

type Device (form : Windows.RenderForm) = 

    let GetDeviceAndSwapChain (form : Windows.RenderForm) =
        let width               = form.ClientSize.Width
        let height              = form.ClientSize.Height

        let mutable desc        = DXGI.SwapChainDescription ()
        desc.BufferCount        <- 2
        desc.ModeDescription    <- SharpDX.DXGI.ModeDescription (
                                    width                           ,
                                    height                          ,
                                    DXGI.Rational (60, 1)           ,
                                    DXGI.Format.R8G8B8A8_UNorm
                                    )
        desc.IsWindowed         <- Bool true
        desc.OutputHandle       <- form.Handle
        desc.SampleDescription  <- DXGI.SampleDescription (1,0)
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

        Direct3D11.Device.CreateWithSwapChain (
            Direct3D.DriverType.Hardware                , 
            Direct3D11.DeviceCreationFlags.BgraSupport  , 
            featureLevels                               , 
            desc                                        , 
            device                                      , swapChain
            )

        !device, !swapChain

    let width               = float32 form.ClientSize.Width
    let height              = float32 form.ClientSize.Height

    let d2dFactory          = new Direct2D1.Factory (Direct2D1.FactoryType.SingleThreaded)
    let device, swapChain   = GetDeviceAndSwapChain form
    let factory             = swapChain.GetParent<DXGI.Factory> ()

    let associateWithWindow = factory.MakeWindowAssociation (form.Handle, DXGI.WindowAssociationFlags.IgnoreAll)

    let backBuffer          = Direct3D11.Texture2D.FromSwapChain<Direct3D11.Texture2D> (swapChain, 0)
//    let persistentBuffer    = new Direct3D11.Texture2D (backBuffer.Device, backBuffer.Description)
    let persistentBuffer    = 
        let mutable description         = Direct3D11.Texture2DDescription()
        description.ArraySize           <- 1
        description.BindFlags           <- Direct3D11.BindFlags.ShaderResource ||| Direct3D11.BindFlags.RenderTarget
        description.CpuAccessFlags      <- Direct3D11.CpuAccessFlags.None
        description.Format              <- DXGI.Format.R8G8B8A8_UNorm
        description.Width               <- int <| width
        description.Height              <- int <| height
        description.MipLevels           <- 1
        description.OptionFlags         <- Direct3D11.ResourceOptionFlags.None
        description.SampleDescription   <- DXGI.SampleDescription(1,0)
        description.Usage               <- Direct3D11.ResourceUsage.Default            
        new Direct3D11.Texture2D (device, description)

    let surface             = backBuffer.QueryInterface<SharpDX.DXGI.Surface> ();
    let persistentSurface   = persistentBuffer.QueryInterface<SharpDX.DXGI.Surface> ();
    let d2dRenderTarget     = new Direct2D1.RenderTarget (
                                d2dFactory                          , 
                                surface                             , 
                                Direct2D1.RenderTargetProperties (
                                    Direct2D1.PixelFormat (
                                        DXGI.Format.Unknown         , 
                                        Direct2D1.AlphaMode.Premultiplied
                                        )
                                    )
                                )
    let d2dPersistentRenderTarget     
                            = new Direct2D1.RenderTarget (
                                d2dFactory                          , 
                                persistentSurface                   , 
                                Direct2D1.RenderTargetProperties (
                                    Direct2D1.PixelFormat (
                                        DXGI.Format.Unknown         , 
                                        Direct2D1.AlphaMode.Premultiplied
                                        )
                                    )
                                )
    let bitmap              = 
        let mutable format      = Direct2D1.PixelFormat()
        format.AlphaMode        <- Direct2D1.AlphaMode.Premultiplied
        format.Format           <- DXGI.Format.R8G8B8A8_UNorm
        let mutable properties  = Direct2D1.BitmapProperties ()
        properties.PixelFormat  <- format
        new Direct2D1.Bitmap (d2dRenderTarget, persistentSurface, Nullable<_> (properties))

    let solid (c : Color)   = new Direct2D1.SolidColorBrush (d2dRenderTarget, c.ToColor4 ())                                    


    let rainbow                 = 
        [|
            0.00F     , Color.Red
            0.30F     , Color.Orange
            0.40F     , Color.Yellow
            0.50F     , Color.Green
            0.60F     , Color.Blue
            0.70F     , Color.Indigo
            1.00F     , Color.Violet
        |]

    let rainbowBrushes          = rainbow |> Array.map (fun (n,c) -> n,solid c)

    let planetBrush             = solid Color.CornflowerBlue

    member x.PlanetBrush        = planetBrush

    member x.GetRainbowBrush (v : Vector2) (c : float32) = 
        let v = v.Length ()
        let r = v / (v + c)
        let i = rainbowBrushes |> Array.findIndex (fun (n,_) -> n > r)
        let fn,fb = rainbowBrushes.[i - 1]
        let sn,sb = rainbowBrushes.[i]

        let o   = (r - fn) / (sn - fn)
        fb.Opacity <- 0.1F * (1.F - o)
        sb.Opacity <- 0.1F * o
        fb :> Direct2D1.Brush, sb :> Direct2D1.Brush



    member x.Width              = width
    member x.Height             = height

    member x.Draw (a : Direct2D1.RenderTarget->Direct2D1.RenderTarget->unit) =
        d2dRenderTarget.BeginDraw ()

        d2dRenderTarget.Clear <| Nullable<_> (Color.Black.ToColor4())
        let rect = RectangleF (-width / 2.F, -height / 2.F, width, height)
        d2dRenderTarget.DrawBitmap (bitmap, rect, 1.F, Direct2D1.BitmapInterpolationMode.Linear)

        d2dPersistentRenderTarget.BeginDraw ()

        try
            a d2dRenderTarget d2dPersistentRenderTarget
        finally
            d2dPersistentRenderTarget.EndDraw ()
            d2dRenderTarget.EndDraw ()
            swapChain.Present (1, DXGI.PresentFlags.None)


    interface IDisposable with
        member x.Dispose () =
            rainbowBrushes 
            |> Array.map (fun (_,b) -> b) 
            |> TryDisposeList

            let resources : IDisposable list = 
                [
                    planetBrush
                    bitmap
                    d2dPersistentRenderTarget
                    d2dRenderTarget
                    persistentSurface
                    surface
                    persistentBuffer
                    backBuffer
                    factory
                    swapChain
                    device
                    d2dFactory
                ]
            resources |> TryDisposeList
            



