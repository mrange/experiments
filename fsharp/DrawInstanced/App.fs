namespace DrawInstanced

open System
open SharpDX
open Common

type App< 'DI 
        , 'DD
        , 'VS when  'DI :> AbstractDeviceIndependent 
              and   'VS : struct 
              and   'VS :> ValueType 
              and   'VS : (new: unit -> 'VS)
              and   'DD :> AbstractDeviceDependent<'VS>
        > (rf : Windows.RenderForm, di : unit -> 'DI, dd : Windows.RenderForm -> 'DI -> 'DD) =
  class
    let deviceIndependent       = di ()
    let mutable deviceDependent = null : AbstractDeviceDependent<_>

    let uninitialize _ =
      printfn "uninitialize"
      dispose "deviceDependent" deviceDependent
      deviceDependent <- null

    let reinitialize _ =
      printfn "reinitialize"
      dispose "deviceDependent" deviceDependent
      deviceDependent <- null
      deviceDependent <- dd rf deviceIndependent

    do
      rf.SizeChanged.Add      reinitialize
      rf.HandleCreated.Add    reinitialize
      rf.HandleDestroyed.Add  uninitialize

    interface IDisposable with
      member x.Dispose () =
        uninitialize ()

    member x.Initialize () =
      ()

    member x.Update timestamp =
      deviceDependent.Update timestamp
      ()

    member x.Render () =
      deviceDependent.Render ()
      ()
  end