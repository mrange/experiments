namespace DrawInstanced

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

  type Vertex (position : Vector3, normal : Vector3, color : Vector4) =
    struct
      member x.Position = position
      member x.Normal   = normal
      member x.Color    = color

      override x.ToString () =
        sprintf "V: %A, %A, %A" position normal color
    end

  type InstanceVertex (position : Vector3, direction : Vector3, rotation : Vector3, delay : Vector3, color : Vector4) =
    struct
      member x.Position   = position
      member x.Direction  = direction
      member x.Rotation   = rotation
      member x.Delay      = delay
      member x.Color      = color

      override x.ToString () =
        sprintf "IV: %A, %A, %A, %A, %A" position direction rotation delay color
    end

  let minz          = -7
  let maxz          = 7
  let alphaz        = true

  let minDelay      = 10.F
  let delayVar      = 20.F
  let totalTime     = 30.F

  let startDistance = 2.F
  let endDistance   = 2.F

  let distance t    = t*(endDistance - startDistance) + startDistance

  let viewPos t     = Vector4 (distance t*1.F, distance t*1.5F, distance t*4.F, 1.F)
  let lightningPos t= Vector4 (-1.F*distance t, -1.F*distance t, 3.F*distance t, 1.F)

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
        let v x y z n i = Vertex (Vector3 (x, y, z), n, Vector4 (i, i, i, 1.F))
        [|
    //       x     y     z    n   i
          v -1.0F -1.0F -1.0F fn  1.0F // Front
          v -1.0F  1.0F -1.0F fn  1.0F
          v  1.0F  1.0F -1.0F fn  1.0F
          v -1.0F -1.0F -1.0F fn  1.0F
          v  1.0F  1.0F -1.0F fn  1.0F
          v  1.0F -1.0F -1.0F fn  1.0F

          v -1.0F -1.0F  1.0F bn 1.0F // Back
          v  1.0F  1.0F  1.0F bn 1.0F
          v -1.0F  1.0F  1.0F bn 1.0F
          v -1.0F -1.0F  1.0F bn 1.0F
          v  1.0F -1.0F  1.0F bn 1.0F
          v  1.0F  1.0F  1.0F bn 1.0F

          v -1.0F -1.0F -1.0F tn 0.5F // Top
          v  1.0F -1.0F  1.0F tn 0.5F
          v -1.0F -1.0F  1.0F tn 0.5F
          v -1.0F -1.0F -1.0F tn 0.5F
          v  1.0F -1.0F -1.0F tn 0.5F
          v  1.0F -1.0F  1.0F tn 0.5F

          v -1.0F  1.0F -1.0F on 0.5F // Bottom
          v -1.0F  1.0F  1.0F on 0.5F
          v  1.0F  1.0F  1.0F on 0.5F
          v -1.0F  1.0F -1.0F on 0.5F
          v  1.0F  1.0F  1.0F on 0.5F
          v  1.0F  1.0F -1.0F on 0.5F

          v -1.0F -1.0F -1.0F ln 0.75F // Left
          v -1.0F -1.0F  1.0F ln 0.75F
          v -1.0F  1.0F  1.0F ln 0.75F
          v -1.0F -1.0F -1.0F ln 0.75F
          v -1.0F  1.0F  1.0F ln 0.75F
          v -1.0F  1.0F -1.0F ln 0.75F

          v  1.0F -1.0F -1.0F rn 0.75F // Right
          v  1.0F  1.0F  1.0F rn 0.75F
          v  1.0F -1.0F  1.0F rn 0.75F
          v  1.0F -1.0F -1.0F rn 0.75F
          v  1.0F  1.0F -1.0F rn 0.75F
          v  1.0F  1.0F  1.0F rn 0.75F
        |]

      let instanceVertices  =
        let m c = float32 c / 255.F

        let v x y z (c : Drawing.Color) =
          InstanceVertex  ( Vector3 (x, y, z)
                          , Vector3.Zero
                          , Vector3.Zero
                          , Vector3.Zero
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
          ie "TEXCOORD"   0 DXGI.Format.R32G32B32_Float     0       1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   1 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   2 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   3 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
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
        let world         = Matrix.Identity
        let worldViewProj = world * view * proj

        viewState.Data <- ViewState (viewPos, lightningPos, world, worldViewProj, Vector4 timestamp)


    end

  let deviceIndependent ()    = new DeviceIndependent ()
  let deviceDependent rf di   = new DeviceDependent (rf, di)
