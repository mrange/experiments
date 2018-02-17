﻿namespace DrawInstanced

open System
open System.Drawing

open SharpDX

open Common

module PixelShaderTest =

  type ViewState  ( viewPos       : Vector4
                  , lightningPos  : Vector4
                  , world         : Matrix
                  , worldViewProj : Matrix
                  , timestamp     : Vector4
                  ) =
    struct
      member x.ViewPos        = viewPos
      member x.LightningPos   = lightningPos
      member x.World          = world
      member x.WorldViewProj  = worldViewProj
      member x.Timestamp      = timestamp
    end

  type Vertex (position : Vector3, normal : Vector3, color : Vector4, texPos : Vector2) =
    struct
      member x.Position = position
      member x.Normal   = normal
      member x.Color    = color
      member x.TexPos   = texPos

      override x.ToString () =
        sprintf "V: %A, %A, %A, %A" position normal color texPos
    end

  type InstanceVertex (position : Vector3, color : Vector4) =
    struct
      member x.Position   = position
      member x.Color      = color

      override x.ToString () =
        sprintf "IV: %A, %A" position color
    end

  let totalTime     = 30.F

  let startDistance = 2.F
  let endDistance   = 1.F

  let distance t    = t*(endDistance - startDistance) + startDistance

  let viewPos t     = Vector4 (distance t*1.F, distance t*1.5F, distance t*4.F, 1.F)
  let lightningPos t= Vector4 (-1.F*distance t, -1.F*distance t, 2.F*distance t, 1.F)

  type DeviceIndependent () =
    class
      inherit AbstractDeviceIndependent ()

      let background    = Color4(0.1F, 0.1F, 0.1F, 1.F)

      let boxVertices  =
        let fn = -Vector3.UnitZ
        let bn = Vector3.UnitZ
        let tn = -Vector3.UnitY
        let on = Vector3.UnitY
        let ln = -Vector3.UnitX
        let rn = Vector3.UnitX
        let v x y z n i tx ty = Vertex (Vector3 (x, y, z), n, Vector4 (i, i, i, 1.F), Vector2 (tx, ty))
        [|
    //       x     y     z    n   i     tx    ty
          v -1.0F -1.0F -1.0F fn  1.0F  0.0F  0.0F    // Front
          v -1.0F  1.0F -1.0F fn  1.0F  0.0F  1.0F
          v  1.0F  1.0F -1.0F fn  1.0F  1.0F  1.0F
          v -1.0F -1.0F -1.0F fn  1.0F  0.0F  0.0F
          v  1.0F  1.0F -1.0F fn  1.0F  1.0F  1.0F
          v  1.0F -1.0F -1.0F fn  1.0F  1.0F  0.0F

          v -1.0F -1.0F  1.0F bn 1.0F   0.0F  0.0F    // Back
          v  1.0F  1.0F  1.0F bn 1.0F   1.0F  1.0F
          v -1.0F  1.0F  1.0F bn 1.0F   0.0F  1.0F
          v -1.0F -1.0F  1.0F bn 1.0F   0.0F  0.0F
          v  1.0F -1.0F  1.0F bn 1.0F   1.0F  0.0F
          v  1.0F  1.0F  1.0F bn 1.0F   1.0F  1.0F

          v -1.0F -1.0F -1.0F tn 0.5F   0.0F  0.0F    // Top
          v  1.0F -1.0F  1.0F tn 0.5F   1.0F  1.0F
          v -1.0F -1.0F  1.0F tn 0.5F   0.0F  1.0F
          v -1.0F -1.0F -1.0F tn 0.5F   0.0F  0.0F
          v  1.0F -1.0F -1.0F tn 0.5F   1.0F  0.0F
          v  1.0F -1.0F  1.0F tn 0.5F   1.0F  1.0F

          v -1.0F  1.0F -1.0F on 0.5F   0.0F  0.0F    // Bottom
          v -1.0F  1.0F  1.0F on 0.5F   0.0F  1.0F
          v  1.0F  1.0F  1.0F on 0.5F   1.0F  1.0F
          v -1.0F  1.0F -1.0F on 0.5F   0.0F  0.0F
          v  1.0F  1.0F  1.0F on 0.5F   1.0F  1.0F
          v  1.0F  1.0F -1.0F on 0.5F   1.0F  0.0F

          v -1.0F -1.0F -1.0F ln 0.75F  0.0F  0.0F    // Left
          v -1.0F -1.0F  1.0F ln 0.75F  0.0F  1.0F
          v -1.0F  1.0F  1.0F ln 0.75F  1.0F  1.0F
          v -1.0F -1.0F -1.0F ln 0.75F  0.0F  0.0F
          v -1.0F  1.0F  1.0F ln 0.75F  1.0F  1.0F
          v -1.0F  1.0F -1.0F ln 0.75F  1.0F  0.0F

          v  1.0F -1.0F -1.0F rn 0.75F  0.0F  0.0F    // Right
          v  1.0F  1.0F  1.0F rn 0.75F  1.0F  1.0F
          v  1.0F -1.0F  1.0F rn 0.75F  0.0F  1.0F
          v  1.0F -1.0F -1.0F rn 0.75F  0.0F  0.0F
          v  1.0F  1.0F -1.0F rn 0.75F  1.0F  0.0F
          v  1.0F  1.0F  1.0F rn 0.75F  1.0F  1.0F

          // TODO: This shouldn't be needed but the last texcoord isn't pickedup
          //  for the last vertex??
          v  0.0F  0.0F  0.0F rn 0.75F  1.0F  1.0F
          v  0.0F  0.0F  0.0F rn 0.75F  1.0F  1.0F
          v  0.0F  0.0F  0.0F rn 0.75F  1.0F  1.0F

        |]

      let instanceVertices  =
        let m c = float32 c / 255.F

        let v x y z (c : Drawing.Color) =
          InstanceVertex  ( Vector3 (x, y, z)
                          , Vector4 (m c.R, m c.G, m c.B, m c.A)
                          )

        let vs =
          [|
            yield v 0.F 0.F 0.F Drawing.Color.White
          |]

        printfn "No of instances: %d" vs.Length

        vs

      let inputElements =
        let aligned = Direct3D12.InputElement.AppendAligned
        let ie name index format offset slot slotClass stepRate =
          Direct3D12.InputElement (name, index, format, offset, slot, slotClass, stepRate)
        [|
          ie "POSITION"   0 DXGI.Format.R32G32B32_Float     0       0 Direct3D12.InputClassification.PerVertexData    0
          ie "NORMAL"     0 DXGI.Format.R32G32B32_Float     aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "COLOR"      0 DXGI.Format.R32G32B32A32_Float  aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "TEXCOORD"   0 DXGI.Format.R32G32B32_Float     aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "TEXCOORD"   1 DXGI.Format.R32G32B32_Float     0       1 Direct3D12.InputClassification.PerInstanceData  1
          ie "COLOR"      1 DXGI.Format.R32G32B32A32_Float  aligned 1 Direct3D12.InputClassification.PerInstanceData  1
        |]

      do
        GC.Collect (2, GCCollectionMode.Forced)

      member x.Background       = background
      member x.BoxVertices      = boxVertices
      member x.InstanceVertices = instanceVertices
      member x.InputElements    = inputElements

      override x.OnDispose ()   = ()

    end


  type DeviceDependent (rf : Windows.RenderForm, di : DeviceIndependent) as this =
    class
      inherit AbstractDeviceDependent<ViewState> (rf, "pixelshadertest.hlsl", di.InputElements)

      let aspectRatio       = this.AspectRatio
      let commandList       = this.CommandList
      let device            = this.Device
      let viewState         = this.ViewState

      // TODO: Dispose these resources after transferred
      let uploadBox         = new UploadVertexBuffer<_> (device, di.BoxVertices)
      let uploadInstance    = new UploadVertexBuffer<_> (device, di.InstanceVertices)

      let defaultBox        = new DefaultVertexBuffer<_> (device, commandList, uploadBox)
      let defaultInstance   = new DefaultVertexBuffer<_> (device, commandList, uploadInstance)

      override x.OnDispose () =
        dispose "defaultInstance"     defaultInstance
        dispose "defaultBox"          defaultBox
        dispose "uploadInstance"      uploadInstance
        dispose "uploadBox"           uploadBox

      override x.OnBackground = rcolor4 di.Background

      override x.OnPopulateCommandList (commandList : Direct3D12.GraphicsCommandList) =
        commandList.PrimitiveTopology <- Direct3D.PrimitiveTopology.TriangleList
        commandList.SetVertexBuffers (0, [|defaultBox.View; defaultInstance.View|], 2)
        commandList.DrawInstanced (defaultBox.Length, defaultInstance.Length, 0, 0)

      override x.OnUpdate (timestamp : float32) =
        let t             = timestamp / totalTime
        let t             = clamp t 0.F 1.F
        let t             = sqrt t
        let t             = sin (float32 pi * t / 2.F)
        let viewPos       = viewPos       t
        let lightningPos  = lightningPos  t
        let viewPosDist   = viewPos.Length ()
        let view          = Matrix.LookAtLH (Vector3 (viewPos.X, viewPos.Y, viewPos.Z), Vector3.Zero, Vector3.Zero - Vector3.UnitY)
        let proj          = Matrix.PerspectiveFovLH (float32 pi / 3.F, aspectRatio, 1.F, 2.F*viewPosDist)
        let world         = Matrix.RotationY 1.0F
        let world         = Matrix.Identity
        let world         = Matrix.RotationY timestamp
        let worldViewProj = world * view * proj

        viewState.Data <- ViewState (viewPos, lightningPos, world, worldViewProj, Vector4 timestamp)


    end

  let deviceIndependent ()    = new DeviceIndependent ()
  let deviceDependent rf di   = new DeviceDependent (rf, di)
