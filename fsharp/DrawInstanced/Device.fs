namespace DrawInstanced

open System
open System.Drawing
open System.Threading

open SharpDX
open SharpDX.Mathematics.Interop

open Common

type CommandList(device : Direct3D12.Device, pipelineState : Direct3D12.PipelineState) =
  class
    let listType   = Direct3D12.CommandListType.Direct
    let allocator  = device.CreateCommandAllocator listType
    let queue      = device.CreateCommandQueue (Direct3D12.CommandQueueDescription listType)
    let list       =
      let cl = device.CreateCommandList (listType, allocator, pipelineState)
      cl.Close () // Opened in recording state, close it.
      cl

    interface IDisposable with
      member x.Dispose () =
        dispose "queue"     queue
        dispose "allocator" allocator

    member x.Execute (a : Direct3D12.GraphicsCommandList -> 'T) : 'T =
      allocator.Reset ()

      list.Reset (allocator, pipelineState)

      let v =
        try
          a list
        finally
          list.Close ()

      queue.ExecuteCommandList list

      v

    member x.Queue = queue
  end

type UploadConstantBuffer<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> ValueType> (device : Direct3D12.Device, initial : 'T) =
  class
    let mutable data = initial
    let size = Utilities.SizeOf<'T> ()

    let heap =
      let dhd = Direct3D12.DescriptorHeapDescription  ( DescriptorCount = 1
                                                      , Flags           = Direct3D12.DescriptorHeapFlags.ShaderVisible
                                                      , Type            = Direct3D12.DescriptorHeapType.ConstantBufferViewShaderResourceViewUnorderedAccessView
                                                      )

      device.CreateDescriptorHeap dhd


    let resource =
      let hp  = Direct3D12.HeapProperties Direct3D12.HeapType.Upload
      let hf  = Direct3D12.HeapFlags.None
      let rd  = Direct3D12.ResourceDescription.Buffer (int64 size)
      let rs  = Direct3D12.ResourceStates.GenericRead

      device.CreateCommittedResource (hp, hf, rd, rs)

    let view =
      let cbvd = Direct3D12.ConstantBufferViewDescription ( BufferLocation  = resource.GPUVirtualAddress
                                                          , SizeInBytes     = ((size + 0xFF) &&& ~~~0xFF)
                                                          )

      device.CreateConstantBufferView (Nullable cbvd, heap.CPUDescriptorHandleForHeapStart)

    let updateConstantBuffer () =
      let ptr = resource.Map 0

      try
        Utilities.Write (ptr, &data)
      finally
        resource.Unmap 0

    do
      updateConstantBuffer ()

    interface IDisposable with
      member x.Dispose () =
        dispose "resource"  resource
        dispose "heap"      heap

    member x.Heap = heap

    member x.Data
      with  get ()  = data
      and   set v   = data <- v; updateConstantBuffer ()
  end

type UploadVertexBuffer<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> ValueType> (device : Direct3D12.Device, initial : 'T []) =
  class
    let data = initial

    let size = Utilities.SizeOf data

    // TODO: Add an extra instance because I had some problems in data being 0
    //  for last vertex. Speculation is that this is related to alignment issues
    let size = size + Utilities.SizeOf<'T> ()

    let resource =
      let hp  = Direct3D12.HeapProperties Direct3D12.HeapType.Upload
      let hf  = Direct3D12.HeapFlags.None
      let rd  = Direct3D12.ResourceDescription.Buffer (int64 size)
      let rs  = Direct3D12.ResourceStates.GenericRead

      device.CreateCommittedResource (hp, hf, rd, rs)

    let view =
      let vbv = Direct3D12.VertexBufferView ( BufferLocation  = resource.GPUVirtualAddress
                                            , StrideInBytes   = Utilities.SizeOf<'T> ()
                                            , SizeInBytes     = size
                                            )

      vbv

    let updateVertexBuffer () =
      let ptr = resource.Map 0

      try
        Utilities.Write (ptr, data, 0, data.Length) |> ignore
      finally
        resource.Unmap 0

    do
      updateVertexBuffer ()

    interface IDisposable with
      member x.Dispose () =
        dispose "resource"  resource

    member x.Data                   = data
    member x.Length                 = data.Length
    member x.Resource               = resource
    member x.Size                   = size
    member x.UpdateVertexBuffer ()  = updateVertexBuffer ()
    member x.View                   = view
  end

type DefaultVertexBuffer<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> ValueType> (device : Direct3D12.Device, commandList: CommandList, initial : UploadVertexBuffer<'T>) =
  class
    let length  = initial.Length
    let size    = initial.Size

    let resource =
      let hp  = Direct3D12.HeapProperties Direct3D12.HeapType.Default
      let hf  = Direct3D12.HeapFlags.None
      let rd  = Direct3D12.ResourceDescription.Buffer (int64 size)
      let rs  = Direct3D12.ResourceStates.GenericRead

      let d   = device.CreateCommittedResource (hp, hf, rd, rs)

      commandList.Execute <| fun commandList ->
        commandList.CopyResource (d, initial.Resource)

      d


    let view =
      let vbv = Direct3D12.VertexBufferView ( BufferLocation  = resource.GPUVirtualAddress
                                            , StrideInBytes   = Utilities.SizeOf<'T> ()
                                            , SizeInBytes     = size
                                            )

      vbv

    interface IDisposable with
      member x.Dispose () =
        dispose "resource"  resource

    member x.Length = length
    member x.Size   = size
    member x.View   = view
  end

type [<AbstractClass>] AbstractDeviceIndependent () =
  class
    abstract OnDispose : unit -> unit

    interface IDisposable with
      member x.Dispose () =
        x.OnDispose ()
  end

type [<AllowNullLiteral; AbstractClass>] AbstractDeviceDependent< 'ViewState  when  'ViewState : struct 
                                                                              and   'ViewState :> ValueType 
                                                                              and   'ViewState : (new: unit -> 'ViewState)
                                                        > ( rf              : Windows.RenderForm
                                                          , shaderFileName  : string
                                                          , ies             : Direct3D12.InputElement []
                                                          ) =
  class

    let size              = rf.ClientSize
    let width             = size.Width
    let height            = size.Height
    let widthf            = width  |> float32
    let heightf           = height |> float32
    let aspectRatio       = widthf / heightf

    let viewPort          = Viewport  (Width = width, Height = height, MaxDepth = 1.F, MinDepth = -1.F)
    let scissorRect       = Rectangle (Width = width, Height = height)

    let device            = new Direct3D12.Device (null, Direct3D.FeatureLevel.Level_11_0)

    let fence             = device.CreateFence (0L, Direct3D12.FenceFlags.None)
    let fenceEvent        = new AutoResetEvent false
    let mutable fenceGen  = 0L

    let rootSignature     =
      let rps =
        [|
          Direct3D12.RootParameter  ( Direct3D12.ShaderVisibility.Vertex
                                    , Direct3D12.DescriptorRange  ( RangeType                         = Direct3D12.DescriptorRangeType.ConstantBufferView
                                                                  , BaseShaderRegister                = 0
                                                                  , OffsetInDescriptorsFromTableStart = Int32.MinValue
                                                                  , DescriptorCount                   = 1
                                                                  )
                                    )
          Direct3D12.RootParameter  ( Direct3D12.ShaderVisibility.Pixel
                                    , Direct3D12.DescriptorRange  ( RangeType                         = Direct3D12.DescriptorRangeType.ConstantBufferView
                                                                  , BaseShaderRegister                = 0
                                                                  , OffsetInDescriptorsFromTableStart = Int32.MinValue
                                                                  , DescriptorCount                   = 1
                                                                  )
                                    )
        |]
      let rsd = Direct3D12.RootSignatureDescription ( Direct3D12.RootSignatureFlags.AllowInputAssemblerInputLayout
                                                    , rps
                                                    )
      use ser = rsd.Serialize ()
      let dp  = DataPointer (ser.BufferPointer, int ser.BufferSize)

      device.CreateRootSignature dp

    let shader main compiler =
  #if DEBUG
      let flags = D3DCompiler.ShaderFlags.Debug
  #else
      let flags = D3DCompiler.ShaderFlags.None
  #endif
      use cff = SharpDX.D3DCompiler.ShaderBytecode.CompileFromFile (shaderFileName, main, compiler, flags)
      use bc  = cff.Bytecode
      if bc = null then
        printfn "Shader error: %s" cff.Message
        failwith "Failed to compile shader"
      Direct3D12.ShaderBytecode bc.Data

    let vertexShader      = shader "VSMain" "vs_5_0"
    let pixelShader       = shader "PSMain" "ps_5_0"

    let pipelineState     =
      let gpsd = Direct3D12.GraphicsPipelineStateDescription( InputLayout           = Direct3D12.InputLayoutDescription ies
                                                            , RootSignature         = rootSignature
                                                            , VertexShader          = vertexShader
                                                            , PixelShader           = pixelShader
                                                            , RasterizerState       = Direct3D12.RasterizerStateDescription.Default ()
                                                            , BlendState            = Direct3D12.BlendStateDescription.Default ()
                                                            , DepthStencilFormat    = DXGI.Format.D32_Float
                                                            , DepthStencilState     = Direct3D12.DepthStencilStateDescription.Default ()
                                                            , SampleMask            = Int32.MaxValue
                                                            , PrimitiveTopologyType = Direct3D12.PrimitiveTopologyType.Triangle
                                                            , RenderTargetCount     = 1
                                                            , Flags                 = Direct3D12.PipelineStateFlags.None
                                                            , SampleDescription     = DXGI.SampleDescription (1, 0)
                                                            , StreamOutput          = Direct3D12.StreamOutputDescription ()
                                                            )

      gpsd.RenderTargetFormats.[0] <- DXGI.Format.R8G8B8A8_UNorm

      device.CreateGraphicsPipelineState gpsd

    let commandList       = new CommandList (device, pipelineState)

    let swapChain         =
      use f   = new DXGI.Factory4 ()
      let scd = DXGI.SwapChainDescription ( BufferCount       = frameCount
                                          , ModeDescription   = DXGI.ModeDescription (width, height, DXGI.Rational (60, 1), DXGI.Format.R8G8B8A8_UNorm)
                                          , Usage             = DXGI.Usage.RenderTargetOutput
                                          , SwapEffect        = DXGI.SwapEffect.FlipDiscard
                                          , OutputHandle      = rf.Handle
                                          // , Flags          = DXGI.SwapChainsFlags.None
                                          , SampleDescription = DXGI.SampleDescription (1, 0)
                                          , IsWindowed        = rtrue
                                          )

      use sw  = new DXGI.SwapChain (f, commandList.Queue, scd)
      let sw3 = sw.QueryInterface<DXGI.SwapChain3> ()

      sw3

    let createHeap dc tp=
      let dhd = Direct3D12.DescriptorHeapDescription  ( DescriptorCount = dc
                                                      , Flags           = Direct3D12.DescriptorHeapFlags.None
                                                      , Type            = tp
                                                      )

      device.CreateDescriptorHeap dhd

    let renderTargetHeap  = createHeap frameCount  Direct3D12.DescriptorHeapType.RenderTargetView

    let renderTargets     =
      let ds  = device.GetDescriptorHandleIncrementSize Direct3D12.DescriptorHeapType.RenderTargetView
      let rts = Array.zeroCreate frameCount

      let rec loop offset i =
        if i < frameCount then
          rts.[i] <- swapChain.GetBackBuffer<Direct3D12.Resource> i
          device.CreateRenderTargetView (rts.[i], Nullable(), offset)
          loop (offset + ds) (i + 1)
      loop renderTargetHeap.CPUDescriptorHandleForHeapStart 0

      rts

    let depthHeap         = createHeap 1 Direct3D12.DescriptorHeapType.DepthStencilView

    let depthBuffer       =
      let hp  = Direct3D12.HeapProperties Direct3D12.HeapType.Default
      let hf  = Direct3D12.HeapFlags.None
      let rd  = Direct3D12.ResourceDescription.Texture2D (DXGI.Format.D32_Float, int64 width, height, int16 1, int16 0, 1, 0, Direct3D12.ResourceFlags.AllowDepthStencil)
      let rs  = Direct3D12.ResourceStates.DepthWrite
      let db  = device.CreateCommittedResource (hp, hf, rd, rs)

      db

    let depthStencilView  =
      let dsvd  = Direct3D12.DepthStencilViewDescription  ( Format    = DXGI.Format.D32_Float
                                                          , Dimension = Direct3D12.DepthStencilViewDimension.Texture2D
                                                          , Flags     = Direct3D12.DepthStencilViewFlags.None
                                                          )

      let dsv   = device.CreateDepthStencilView (depthBuffer, Nullable dsvd, depthHeap.CPUDescriptorHandleForHeapStart)

      dsv

    let viewState  = new UploadConstantBuffer<_> (device, new 'ViewState ())

    let waitForPreviousFrame () =
      // TODO: Find other way to await frame
      let localFenceGen = fenceGen
      fenceGen          <-fenceGen + 1L
      commandList.Queue.Signal (fence, localFenceGen)

      while (fence.CompletedValue < localFenceGen) do
        fence.SetEventOnCompletion (localFenceGen, fenceEvent.SafeWaitHandle.DangerousGetHandle ())
        fenceEvent.WaitOne () |> ignore

    abstract OnDispose              : unit -> unit
    abstract OnBackground           : RawColor4
    abstract OnPopulateCommandList  : Direct3D12.GraphicsCommandList -> unit
    abstract OnUpdate               : float32 -> unit

    member x.PopulateCommandList (commandList : Direct3D12.GraphicsCommandList) =
      commandList.SetGraphicsRootSignature  rootSignature
      commandList.SetViewport               (rviewPortf viewPort)
      commandList.SetScissorRectangles      [|rrectangle scissorRect|]

      commandList.SetDescriptorHeaps (1, [| viewState.Heap |]);
      commandList.SetGraphicsRootDescriptorTable (0, viewState.Heap.GPUDescriptorHandleForHeapStart)
      commandList.SetGraphicsRootDescriptorTable (1, viewState.Heap.GPUDescriptorHandleForHeapStart)

      let frameIndex  = swapChain.CurrentBackBufferIndex

      commandList.ResourceBarrierTransition (renderTargets.[frameIndex], Direct3D12.ResourceStates.Present, Direct3D12.ResourceStates.RenderTarget)

      let ds          = device.GetDescriptorHandleIncrementSize Direct3D12.DescriptorHeapType.RenderTargetView
      let rtvOffset   = renderTargetHeap.CPUDescriptorHandleForHeapStart + frameIndex*ds
      let depthOffset = depthHeap.CPUDescriptorHandleForHeapStart

      commandList.SetRenderTargets (Nullable rtvOffset, Nullable depthOffset)

      commandList.ClearRenderTargetView (rtvOffset, x.OnBackground, 0, null)
      commandList.ClearDepthStencilView (depthOffset, Direct3D12.ClearFlags.FlagsDepth, 1.F, 0uy, 0, null)

      x.OnPopulateCommandList commandList

      commandList.ResourceBarrierTransition (renderTargets.[frameIndex], Direct3D12.ResourceStates.RenderTarget, Direct3D12.ResourceStates.Present)

    member x.Update timestamp = x.OnUpdate timestamp

    member x.Render () =
      try
        commandList.Execute x.PopulateCommandList

        swapChain.Present (1, DXGI.PresentFlags.None) |> ignore

        waitForPreviousFrame ()
      with
      | e ->
        let result = device.DeviceRemovedReason
        printfn "Device removed: %A" result
        reraise ()

    member x.AspectRatio    = aspectRatio
    member x.CommandList    = commandList
    member x.Device         = device
    member x.ViewState      = viewState

    interface IDisposable with
      member x.Dispose () =
        x.OnDispose ()
        dispose "viewState"           viewState
        dispose "depthBuffer"         depthBuffer
        dispose "depthHeap"           depthHeap
        for rt in renderTargets do
          dispose "renderTarget"      rt
        dispose "renderTargetHeap"    renderTargetHeap
        dispose "swapChain"           swapChain
        dispose "fenceEvent"          fenceEvent
        dispose "fence"               fence
        dispose "commandList"         commandList
        dispose "pipelineState"       pipelineState
        dispose "rootSignature"       rootSignature
        dispose "device"              device
  end
