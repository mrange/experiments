namespace TurtlePower

open Turtle

module TreeFractal =

    let LeftScaling     = 1.F / sqrt 2.F
    let RightScaling    = 1.1F * LeftScaling
    
    let GenerateFlower v = 
        turtle {
            do! Color MediumVioletRed
            do! Forward v
            do! Turn 90.F
            do! Forward (v / 2.F)
            do! Turn -120.F
            do! Forward v
            do! Turn -120.F
            do! Forward v
            do! Turn -120.F
            do! Forward (v / 2.F)
        }

    let rec GenerateBranch n v time a = 
        turtle {
            do! Turn a
            do! Generate (n - 1) v time
        }
    and Generate n v time = 
        turtle {
            if n <= 0 then
                do! GenerateFlower v
            else
                let a = min 20.F <| 1.F + time
                let c = 
                    match n with
                    | nn when nn < 3    -> Lime
                    | nn when nn < 6    -> LimeGreen
                    | _                 -> Brown

                do! Color c
                do! Turn <| 10.F * (sin <| 20.F * Deg2Rad * time)
                do! Width <| float32 n
                do! Forward v
                do! RunAndReturn <| GenerateBranch n (v * LeftScaling) time a
                do! GenerateBranch n (v * RightScaling) time -a
        }

module RecursiveBox = 

    let Generate v time = 
        turtle {
            let d = v + 50.0F * (sin <| 20.F * Deg2Rad * time)
            do! Forward v
            do! Forward v
            for i in 0..200 do
                do! Turn -88.F
                let divisor = pown 1.02F i
                do! Forward <| d / divisor
        }


module Box = 

    let Generate v time = 
        turtle {
            do! Forward v
            do! Forward v
            for i in 0..3 do
                do! Turn 90.F
                do! Forward v
        }
        