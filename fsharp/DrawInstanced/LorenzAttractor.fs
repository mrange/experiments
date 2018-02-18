namespace DrawInstanced

open System
open System.Drawing

open SharpDX

open Common

module LorenzAttractor =

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
        sprintf "V: p:%A, n:%A, c:%A, x:%A" position normal color texPos
    end

  type InstanceVertex ( color         : Vector4
                      , position      : Vector3
                      , nextPosition  : Vector3
                      , scale         : Vector3
                      , nextScale     : Vector3
                      , rotation      : Vector3
                      , nextRotation  : Vector3
                      ) =
    struct
      member x.Color        = color
      member x.Position     = position
      member x.NextPosition = nextPosition
      member x.Scale        = scale
      member x.NextScale    = nextScale
      member x.Rotation     = rotation
      member x.NextRotation = nextRotation

      member x.WithPosition     p = InstanceVertex (color, p, nextPosition, scale, nextScale, rotation, nextRotation)
      member x.WithNextPosition p = InstanceVertex (color, position, p, scale, nextScale, rotation, nextRotation)

      override x.ToString () =
        sprintf "IV: c:%A, p:%A, np:%A, s:%A, ns:%A, r:%A, nr:%A" color position nextPosition scale nextScale rotation nextRotation
    end

  let totalTime         = 30.F

  let startDistance     = 10.F
  let endDistance       = 2.F

  let distance t        = t*(endDistance - startDistance) + startDistance

  let viewPos  t        = Vector3 (-1.0F*distance t, +1.5F*distance t, 4.0F*distance t)
  let lightningPos t    = Vector3 (-1.0F*distance t, -1.0F*distance t, 2.0F*distance t)

  let toV4 (v : Vector3) : Vector4 = Vector4(v.X, v.Y, v.Z, 1.F)

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

          let vx v tp = Vertex (v, nor, col, tp)
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
        let v3 x y z = Vector3 (float32 x, float32 y, float32 z)

        let iv p np s ns r nr c =
          InstanceVertex  ( c
                          , p
                          , np
                          , s
                          , ns
                          , r
                          , nr
                          )

        let inline xyz (v : Vector3) = float v.X, float v.Y, float v.Z

        let slow  = Vector4 (1.F, 0.F, 0.F, 1.F)
        let fast  = Vector4 (0.F, 1.F, 0.F, 1.F)

        let vs =
          let total = 200000
          let rho   = 28.
          let sigma = 10.
          let beta  = 8./3.
          let td    = 0.0005
          let rec loop p s r n maxv minv vs =
            let x, y, z = xyz p
            if n > 0 then
              let dx  = sigma*(y - x)
              let dy  = x*(rho - z) - y
              let dz  = x*y - beta*z
              let vv  = sqrt (dx*dx + dy*dy + dz*dz)
              let maxv= max vv maxv
              let minv= min vv minv
              let ss  = vv / 10000.
              let col = lerp (float32 (sqrt (vv - 15.)) / sqrt 400.F) slow fast
              let xx  = x + td*dx
              let yy  = y + td*dy
              let zz  = z + td*dz
              let np  = v3 xx yy zz
              let ns  = v3 ss ss ss
              let nr  = r + v3 0.2 0.4 0.6
              let vs  = (iv p np s ns r nr col)::vs
              loop np ns nr (n - 1) maxv minv vs
            else
              vs, maxv, minv
          let p = v3 1. 1. 1.
          let r = v3 0. 0. 0.
          let s = v3 0. 0. 0.

          let vs, maxv, minv = loop p r s total Double.MinValue Double.MaxValue []

          printfn "Max speed: %A" maxv
          printfn "Min speed: %A" minv

          vs |> List.toArray

        printfn "No of instances: %d" vs.Length

        let avgPosition =
          let folder s (v : InstanceVertex) = s + v.Position
          let v = 
            vs
            |> Array.fold folder Vector3.Zero
          (1.F / float32 vs.Length) * v

        printfn "Avg position: %A" avgPosition

        vs 
        |> Array.map (fun v -> 
          let v = v.WithPosition (v.Position - avgPosition)
          let v = v.WithNextPosition(v.NextPosition - avgPosition)
          v
          )


      let inputElements =
        let aligned = Direct3D12.InputElement.AppendAligned
        let ie name index format offset slot slotClass stepRate =
          Direct3D12.InputElement (name, index, format, offset, slot, slotClass, stepRate)
        [|
          ie "POSITION"   0 DXGI.Format.R32G32B32_Float     0       0 Direct3D12.InputClassification.PerVertexData    0
          ie "NORMAL"     0 DXGI.Format.R32G32B32_Float     aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "COLOR"      0 DXGI.Format.R32G32B32A32_Float  aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "TEXCOORD"   0 DXGI.Format.R32G32_Float        aligned 0 Direct3D12.InputClassification.PerVertexData    0
          ie "COLOR"      1 DXGI.Format.R32G32B32A32_Float  0       1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   1 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   2 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   3 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   4 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   5 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
          ie "TEXCOORD"   6 DXGI.Format.R32G32B32_Float     aligned 1 Direct3D12.InputClassification.PerInstanceData  1
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
      inherit AbstractDeviceDependent<ViewState> (rf, "lorenzattractor.hlsl", di.InputElements)

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
        let viewPos       = viewPos      t
        let lightningPos  = lightningPos t
        let view          = Matrix.LookAtLH (viewPos, Vector3.Zero, Vector3.Zero - Vector3.UnitY)
        let proj          = Matrix.PerspectiveFovLH (float32 pi / 3.F, aspectRatio, 0.1F, Single.MaxValue)
        let world         = Matrix.RotationY (3.F * (1.F - t))
        let worldViewProj = world * view * proj

        viewState.Data <- ViewState (toV4 viewPos, toV4 lightningPos, world, worldViewProj, Vector4 (2.0F * timestamp))


    end

  let deviceIndependent ()    = new DeviceIndependent ()
  let deviceDependent rf di   = new DeviceDependent (rf, di)
