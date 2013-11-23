open mst
open mst.test

open System
open System.Windows

open MSPaint

type DrawFractal = 
    |   Plant  
    |   Tree
    |   OtherTree
    |   SierpinskiTriangle

[<EntryPoint>]
let main argv = 

    let mySimpleScenario = scenario {
        do! StartMSPaint

        do! DrawOval        100. 100. 200. 200.
        do! DrawRectangle   200. 200. 100. 100.
        do! DrawLine        300. 300. 200. 200.

        do! SaveFile "tester.png"

        do! Scenario.Pause  2000
        }

    let myScenario drawFractal = scenario {
        do! StartMSPaint

        do! Fill (Color.New 240 240 240) 40. 40.

        do! SelectTool "Line"

        let! bounds = GetDrawingBounds

        let points : (float*Vector*Vector) list ref = ref []

        let generator,start,direction = 
            match drawFractal with
            | SierpinskiTriangle    ->
                SierpinskiTriangleFractal.Generate 6 500.   ,
                Vector()                                    ,
                Vector(1., 0.)
            | Plant                 ->
                PlantFractal.Generate 4 80.                 ,   
                Vector()                                    ,   
                Vector(1., -2.)
            | Tree                  ->
                TreeFractal.Generate 5 80.                  ,   
                Vector()                                    ,   
                Vector(0., -1.)
            | OtherTree             ->
                OtherTreeFractal.Generate 5 80.             ,   
                Vector()                                    ,   
                Vector(0., -1.)
                
        ignore <| Turtle.Execute 4. start direction (fun w f t -> points := (w,f,t)::!points) generator

        let insert = 60.

        let mx,my = 
            !points 
            |> List.fold 
                (fun (mx,my) (_,f,t) -> 
                    let mx = min mx <| min f.X t.X
                    let my = min my <| min f.Y t.Y
                    mx,my
                ) 
                (Double.MaxValue, Double.MaxValue)

        let gs = 
            !points 
            |> List.toSeq
            |> Seq.map (fun (w,f,t) -> 
                let lz = 
                    match w with
                    | ww when ww <= 2.  -> Px1, Color.New 000 200 000
                    | ww when ww <= 4.  -> Px3, Color.New 000 128 000
                    | ww when ww <= 6.5 -> Px5, Color.New 000 064 000
                    | _                 -> Px8, Color.New 128 064 000
                let f = Vector(f.X - mx + insert, f.Y - my + insert)
                let t = Vector(t.X - mx + insert, t.Y - my + insert)
                lz,f,t
                )
            |> Seq.groupBy (fun (lz,_,_) -> lz)
            |> Seq.toArray
            |> Array.rev

        for ((lz,c),ps) in gs do 
            do! SelectColor c
            do! SelectSize lz
            for (_,f,t) in ps do
                do! Draw bounds f t
                do! Scenario.Pause  50

        do! SaveFile "tester.png"

        do! Scenario.Pause  2000
        }

    let run = Scenario.RunScenario Map.empty <| myScenario OtherTree

    for result in run.State.Results do
        printfn "Result: %A" result

    0
