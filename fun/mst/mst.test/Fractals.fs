namespace mst.test

open Turtle

module PlantFractal =
    
    let Left = -25.
    let Right = 25.

    let rec GenerateSubTree n v = 
        turtle {
            do! Generate (n - 1) (v / 2.)
        }
    and GenerateRotatedSubTree n v a = 
        turtle {
            do! Turn a
            do! GenerateSubTree n v
        }
    and GenerateBranch n v = 
        turtle {
            do! Turn Right
            do! Forward v
            do! GenerateSubTree n v
        }
    and Generate n v = 
        turtle {
            if n <= 0 then
                return ()
            else
                do! Width <| (1.5 * float n)
                do! Forward v
                do! Turn Left
                do! RunAndReturn <| GenerateSubTree n v
                do! RunAndReturn <| GenerateRotatedSubTree n v Right
                do! Turn Right
                do! Forward v
                do! RunAndReturn <| GenerateBranch n v
                do! Turn Left
                do! RunAndReturn <| GenerateSubTree n v
        }
     

module SierpinskiTriangleFractal = 

    let Left = -120.
    let Right = 120.

    let rec GenerateSubTree n v =

        turtle {
            if n <= 0 then
                do! Forward <| 2. * v
                return ()
            else
               do! GenerateSubTree (n - 1) (v / 2.)
               do! Turn Left
               do! Forward v
               do! Turn Right
               do! GenerateSubTree (n - 1) (v / 2.)
               do! Turn Right
               do! Forward v
               do! Turn Left
               do! GenerateSubTree (n - 1) (v / 2.)
        }

    let  Generate n v =

        turtle {
               do! Width <| 1.
               do! GenerateSubTree (n - 1) (v / 2.)
               do! Turn Left
               do! Forward v
               do! Turn Left
               do! Forward v
        }


module TreeFractal =
    
    let Left    = -90.
    let Right   = 120.

    let Scaling = 1.5


    let rec GenerateBranch n v a = 
        turtle {
            do! Turn (a / (float n))
            do! Forward v
            do! Generate (n - 1) (v / Scaling)
        }
    and Generate n v = 
        turtle {
            if n <= 0 then
                return ()
            else
                do! Width <| (1.5 * float n)
                do! Forward v
                do! RunAndReturn <| GenerateBranch n v Right
                do! Forward v
                do! RunAndReturn <| GenerateBranch n v Left
                do! GenerateBranch n v Right

        }
     

