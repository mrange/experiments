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

  type Vertex (position : Vector3, normal : Vector3, tangent : Vector3, binormal : Vector3, color : Vector4, texPos : Vector2) =
    struct
      member x.Position = position
      member x.Normal   = normal
      member x.Tangent  = tangent
      member x.Binormal = binormal
      member x.Color    = color
      member x.TexPos   = texPos

      override x.ToString () =
        sprintf "V: p:%A, n:%A, t:%A, b:%A, c:%A, x:%A" position normal tangent binormal color texPos
    end

  type InstanceVertex (position : Vector3, color : Vector4) =
    struct
      member x.Position   = position
      member x.Color      = color

      override x.ToString () =
        sprintf "IV: p:%A, c:%A" position color
    end

  let totalTime     = 30.F

  let startDistance = 2.F
  let endDistance   = 1.F

  let distance t    = t*(endDistance - startDistance) + startDistance

  let viewPos t     = Vector4 (-1.0F*distance t, +1.5F*distance t, 4.0F*distance t, 1.F)
  let lightningPos t= Vector4 (-1.0F*distance t, -1.0F*distance t, 2.0F*distance t, 1.F)

  type DeviceIndependent () =
    class
      inherit AbstractDeviceIndependent ()

      let background    = Color4(0.1F, 0.1F, 0.1F, 1.F)

      let boxVertices  =
        let v rot x y z =
          let x, y, z = rot x y z
          Vector3 (x, y, z)
        let t x y = Vector2 (x, y)
        let quad rot c : Vertex [] =
          let v00 = v rot -1.0F -1.0F -1.0F
          let v01 = v rot -1.0F +1.0F -1.0F
          let v10 = v rot +1.0F -1.0F -1.0F
          let v11 = v rot +1.0F +1.0F -1.0F
          let t00 = t +0.0F +0.0F
          let t01 = t +0.0F +1.0F
          let t10 = t +1.0F +0.0F
          let t11 = t +1.0F +1.0F
          let tng = v10 - v00 |> normalize
          let bi  = v01 - v00 |> normalize
          let nor = -Vector3.Cross (tng, bi) |> normalize
          let col = Vector4 (c, c, c, 1.F)

          let vx v tp = Vertex (v, nor, tng, bi, col, tp)
          [|
            vx v00 t00
            vx v01 t01
            vx v11 t11
            vx v00 t00
            vx v11 t11
            vx v10 t10
          |]

        let rot00 x y z = +x, +y, +z
        let rot01 x y z = +x, +z, -y
        let rot03 x y z = +x, -z, +y
        let rot10 x y z = +z, +y, -x
        let rot20 x y z = -x, +y, -z
        let rot30 x y z = -z, +y, +x

        [|
//                    rot   o
          yield! quad rot00 +1.0F  // Front
          yield! quad rot20 +1.0F  // Back
          yield! quad rot01 +0.5F  // Top
          yield! quad rot03 +0.5F  // Bottom
          yield! quad rot10 +0.75F // Left
          yield! quad rot30 +0.75F // Right
        |]

      let instanceVertices  =
        let m c = float32 c / 255.F

        let v x y z (c : Drawing.Color) =
          InstanceVertex  ( Vector3 (x, y, z)
                          , Vector4 (m c.R, m c.G, m c.B, m c.A)
                          )

        let vs =
          [|
            yield v 0.F  0.F 0.F Drawing.Color.White
            yield v 0.F  3.F 0.F Drawing.Color.White
            yield v 0.F -3.F 0.F Drawing.Color.White
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
          ie "TANGENT"    0 DXGI.Format.R32G32B32_Float     aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "BINORMAL"   0 DXGI.Format.R32G32B32_Float     aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "COLOR"      0 DXGI.Format.R32G32B32A32_Float  aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "TEXCOORD"   0 DXGI.Format.R32G32_Float        aligned 0 Direct3D12.InputClassification.PerVertexData    0
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
