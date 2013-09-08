open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Threading

open RayTracer


let White   = Color.New 1. 1. 1.
let Red     = Color.New 1. 0. 0.
let Green   = Color.New 0. 1. 0.
let Blue    = Color.New 0. 0. 1.
let Black   = Color.New 0. 0. 0.



[<EntryPoint>]
[<STAThread>]
let main argv = 

    let lights = 
       [|
            LightSource.New (White.Dim 0.75) (Vector3.New 2. 3. 2.)
            LightSource.New (White.Dim 0.25) (Vector3.New 5. 3. 0.)
       |]

    let world = 
        [|
            Plane(UniformSurface  <| Matte (White.Dim 0.75) , 0., Vector3.New 0. 1. 0.).AsShape
            Sphere(UniformSurface <| Matte Red  , Vector3.New 0. 1. 0., 1.).AsShape
            Sphere(UniformSurface <| Reflective 0.60 Green, Vector3.New 2. 0.5 1., 0.5).AsShape
            Sphere(UniformSurface <| Reflective 0.60 Blue , Vector3.New 3. 0.5 0., 0.5).AsShape
        |]

    let eye         = Vector3.New 6. 1. 0.
    let at          = Vector3.New 0. 1. 0.
    let clipDistance= 1.
    let clipUp      = Vector3.New 0. 1. 0.
    let fov         = degree2rad 120.

    let window = new Window()
    window.MinWidth <- 640.
    window.MinHeight <- 400.


    use loaded = window.Loaded.Subscribe (fun v -> 
        let width   = window.Width  / 4.
        let height  = window.Height / 4.
        let ratio   = width / height

        let wb = new WriteableBitmap(int width, int height, 96., 96., PixelFormats.Bgr32, null)

        let iwidth = wb.PixelWidth
        let iheight = wb.PixelHeight

        let viewPort = ViewPort.New eye at clipUp clipDistance fov ((float iheight) / (float iwidth))

        let tracer = 
            async {
                
                let row = [| for i in 0..iheight - 1 -> Black|]

                for x in 0..iwidth - 1 do
                    for y in 0..iheight - 1 do
                        let vp = viewPort.Corner3 + viewPort.Axis0.Scale (viewPort.Width * float x / width) - viewPort.Axis1.Scale (viewPort.Height * float y / height)
                        let ray = Ray.FromTo eye vp
                        row.[y] <- Trace ray world lights
    
                    dispatch window.Dispatcher (fun () -> 
                        let pixels = 
                            [| for i in row do
                                yield asByte i.Blue
                                yield asByte i.Green
                                yield asByte i.Red 
                                yield byte 255
                            |]
                        wb.WritePixels (Int32Rect (x, 0, 1, iheight), pixels, 4, 0)
                        )

                return ()
            }

        Async.Start tracer

        let i = new Image ()
        i.Source <- wb
        window.Content <- i
        )

    let result = window.ShowDialog()

    0
