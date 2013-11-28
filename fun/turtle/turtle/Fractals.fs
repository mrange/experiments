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
                let a = 15.F
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

