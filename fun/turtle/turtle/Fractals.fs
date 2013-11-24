namespace TurtlePower

open Turtle

module TreeFractal =
    
    let rec GenerateBranch n v s t a = 
        turtle {
            do! Turn a
            do! Generate (n - 1) (v / s) s t
        }
    and Generate n v s t = 
        turtle {
            if n <= 0 then
                return ()
            else
                do! Forward v
//                do! Turn 5.F
                do! RunAndReturn <| GenerateBranch n v s t t
                do! RunAndReturn <| GenerateBranch n v s t -t
        }


