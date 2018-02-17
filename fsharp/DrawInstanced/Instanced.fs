namespace DrawInstanced

open System
open System.Drawing

open SharpDX

open Common

module Instanced =

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
  let totalTime     = minDelay + delayVar

  let startDistance = 1200.F
  let endDistance   = 220.F

  let distance t    = t*(endDistance - startDistance) + startDistance

  let viewPos t     = Vector4 (0.F, distance t*1.5F, distance t*4.F, 1.F)
  let lightningPos t= Vector4 (-1.F*distance t, -1.F*distance t, 3.F*distance t, 1.F)

  type DeviceIndependent () =
    class
      inherit AbstractDeviceIndependent ()

//  let background    = Color4(0.1F, 0.1F, 0.1F, 1.F)
//  let background    = Color4(0.2F, 0.025F, 0.15F, 1.F)
      let background    = Color4(0.1F, 0.1F, 0.1F, 1.F)
//  let background    = Color4(1.F, 1.F, 1.F, 1.F)

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

#if DD
      let instanceVertices =
        let depth     = 4
        let boxCount  = pown 20 depth
        let maxSide   = (pown 3.F depth)
        let maxDist   = sqrt (3.F * (pown (maxSide / 2.F) 2))

        let v x y z c =
          let v     = Vector3 (x, y, z)
          let s     = Vector3 2.F
          let ratio = v.Length () / maxDist
          let delay = minDelay + random.NextFloat(0.F, 1.F) * ratio * delayVar
          InstanceVertex  ( s * Vector3 (x, y, z)
                          , s * s * randomVector3 ()
                          , randomVector3 ()
                          , delay * Vector3.UnitX
                          , c
                          )

        let ra = ResizeArray<InstanceVertex> boxCount

        let rec menger_cube x y z i =
          if i > 0 then
            let d = pown 3.F (i - 1)
            let i = i - 1
            for xx = -1 to 1 do
              for yy = -1 to 1 do
                for zz = -1 to 1 do
                  let a = abs xx + abs yy + abs zz
                  if a > 1 then
                    let xx = x + d*float32 xx
                    let yy = y + d*float32 yy
                    let zz = z + d*float32 zz
                    menger_cube xx yy zz i
          else
            let c c = 
              let c = abs c / (maxSide / 2.F)
              c*c*c
            let cc = Vector4 (c x, c y, c z, 1.F)
            ra.Add (v x y z cc)

        menger_cube 0.F 0.F 0.F depth

        let vs = ra.ToArray ()

        printfn "Instance count: %d" vs.Length

        vs
#else
      let instanceVertices  =
        use bmp   = new Bitmap ("img.png")

        let h     = bmp.Height
        let w     = bmp.Width
        let d     = maxz - minz + 1

        let trs   = Drawing.Color.FromArgb (0,0,0,0)
        let ins   = Drawing.Color.FromArgb (1,0,0,0)

        let pixels3 = Array3D.init w h d (fun x y z ->
          let c   = bmp.GetPixel (x, y)
          let i   = max (max c.R c.G) c.B
          let rz  = float d * float i / 255.0 |> round |> int
          let tz  = rz
          if (not alphaz || z <= tz) && c.A = 255uy then
            c
          else
            trs
          )

        let isVisible x y z =
          let c = pixels3.[x,y,z]
          c.A > 0uy

        for x = 1 to (w - 2) do
          for y = 1 to (h - 2) do
            for z = 1 to (d - 2) do
              let c = pixels3.[x,y,z]
              let nc=
                if c.A > 0uy then
                  let isInside =
                    true
                    &&  isVisible (x - 1) y z
                    &&  isVisible (x + 1) y z
                    &&  isVisible x (y - 1) z
                    &&  isVisible x (y + 1) z
                    &&  isVisible x y (z - 1)
                    &&  isVisible x y (z + 1)
                  if isInside then
                    ins
                  else
                    c
                else
                  c
              pixels3.[x,y,z] <- nc

        let pixels =
          [|
            for x = 0 to w - 1 do
              for y = 0 to h - 1 do
                for z = 0 to (d - 1) do
                  let c = pixels3.[x,y,z]
                  if (c.A = 255uy) then
                    yield struct (x - w / 2 |> float32, y - h / 2 |> float32, z + minz |> float32, c)
          |]

        let maxDist =
          pixels
          |> Seq.map (fun struct (x, y, _, _) -> sqrt (x*x + y*y))
          |> Seq.max

        let min = Vector3 minDelay

        let m c = float32 c / 255.F

        let s   = Vector3 2.F

        let v x y z (c : Drawing.Color) =
          let ratio     = sqrt (x*x + y*y) / maxDist
          let vx        = x / maxDist
          let vy        = y / maxDist
          let vz        = z / maxDist
          let vector    = Vector3 (vx, vy, vz)
    //      let delay     = min + random.NextFloat(0.F, 1.F) * ratio * delayVar
          let delay     = min + ratio * delayVar
          let rotaxis t = (((t * 4.F) % 1.F) - 0.5F)*2.F*pi
          let rot       = Matrix.RotationX (rotaxis vx)*Matrix.RotationY (rotaxis vy)*Matrix.RotationZ (rotaxis vz)
          let vector    = transform rot vector

          InstanceVertex  ( s * Vector3 (x, y, z)
                          , s * s * vector + s*s
                          , vector
                          , delay * Vector3.UnitX
                          , Vector4 (m c.R, m c.G, m c.B, m c.A)
                          )


        let vs =
          [|
            for struct (x, y, z, c) in pixels do
                yield v x y z c
          |]

        printfn "No of instances: %d" vs.Length

        vs
#endif

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
      inherit AbstractDeviceDependent<ViewState> (rf, "instanced.hlsl", di.InputElements)

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
        let world         = Matrix.RotationY (-6.F*(1.F - t) + 0.5F)*Matrix.RotationX (-3.F*(1.F - t))*Matrix.RotationZ (-2.F*(1.F - t))
    //    let world         = Matrix.Identity
        let worldViewProj = world * view * proj

        viewState.Data <- ViewState (viewPos, lightningPos, world, worldViewProj, Vector4 timestamp)


    end

  let deviceIndependent ()    = new DeviceIndependent ()
  let deviceDependent rf di   = new DeviceDependent (rf, di)
