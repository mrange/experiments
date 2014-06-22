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
open System.Threading
open System.Collections.Generic

open SharpDX

open Gravity

module Window = 

    type Message = 
        {
            Reply       : RenderParticle [] -> unit
        }
        static member New r = {Reply = r}

    let Show (particles : Particle []) = 
        let gravityProcessor (particles : Particle []) (ct : CancellationToken) (input : MailboxProcessor<Message>) : Async<unit> =
            async {
                let particles = ref <| Cleanup particles
                while not ct.IsCancellationRequested do
                    let! message    = input.Receive ()
                    let ps,rp       = TimeStep (1.F / 60.F) !particles
                    particles       := ps
                    message.Reply rp
            }
        
        let averageSpeed        = (particles |> Array.sumBy (fun p -> p.Velocity.Length ())) / (float32 particles.Length)

        use form                = new Windows.RenderForm ("Turtle Power")

        form.ClientSize         <- System.Drawing.Size (1600,1200)

        use d2dFactory          = new Direct2D1.Factory (Direct2D1.FactoryType.SingleThreaded)

        let device              = ref <| new Device (form)

        let disposeDevice ()    = TryRun (upcast !device : IDisposable).Dispose
        let recreateDevice ()   = disposeDevice ()
                                  device := new Device (form)

        use onExitDisposeDevice = OnExit disposeDevice

        use cts = new CancellationTokenSource ()
        let ct = cts.Token

        let renderParticles     = ref <| (particles |> ToRenderParticles)

        use mp = MailboxProcessor.Start (gravityProcessor particles ct, ct)

        use onExitCancelTask    = OnExit cts.Cancel

        let resizer             = EventHandler (fun o e -> recreateDevice ())

        form.Resize.AddHandler  resizer

        use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

        Windows.RenderLoop.Run (form, fun () -> 

            let message = Message.New <| fun rp -> renderParticles := rp
            mp.Post message
            
            let d = !device

            d.Draw <| fun target persistentTarget -> 
                
//                target.Clear (Nullable<_> (Color.Black.ToColor4 ()))

                let ps      = !renderParticles

                let max     = ps |> Array.maxBy (fun p -> p.Mass)
                let pos     = max.Current

                let transform = 
                    Matrix3x2.Identity 
//                    <*> Matrix3x2.Rotation (Deg2Rad * 180.F)
                    <*> Matrix3x2.Translation (d.Width/2.F, d.Height/2.F) 
//                    <*> Matrix3x2.Translation (-pos.X, -pos.Y) 
                target.Transform <- transform
                persistentTarget.Transform <- transform

                let ps  = !renderParticles
                let last= ps.Length - 1
                for i in 0..last do
                    let p = ps.[i]
                    let ellipse = Direct2D1.Ellipse (p.Current, p.Radius, p.Radius)
                    let b = d.PlanetBrush
                    b.Opacity <- 1.F
                    target.FillEllipse (ellipse, b)
                    if max = p then ()
                    else
                        let f,s = d.GetRainbowBrush p.Velocity averageSpeed
                        persistentTarget.DrawEllipse (ellipse, f, 1.F)
                        persistentTarget.DrawEllipse (ellipse, s, 1.F)
        )





