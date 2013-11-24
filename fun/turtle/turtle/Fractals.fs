namespace TurtlePower

open Turtle

module TreeFractal =

    let LeftScaling     = 1.F / sqrt 2.F
    let RightScaling    = 1.1F * LeftScaling
    
    let rec GenerateBranch n v time a = 
        turtle {
            do! Turn a
            do! Generate (n - 1) v time
        }
    and Generate n v time = 
        turtle {
            if n <= 0 then
                return ()
            else
                let a = (5.F * time) % 180.F
                let c = 
                    match n with
                    | nn when nn < 3    -> Lime
                    | nn when nn < 6    -> LimeGreen
                    | _                 -> Brown

                do! Color c
                do! Turn -5.F
                do! Width <| float32 n
                do! Forward v
                do! RunAndReturn <| GenerateBranch n (v * LeftScaling) time a
                do! GenerateBranch n (v * RightScaling) time (-a - 5.F)
        }

