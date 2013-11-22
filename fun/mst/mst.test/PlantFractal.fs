namespace mst.test

module PlantFractal =
    
    let Left = -25.
    let Right = 25.

    let rec GenerateSubTree n v = 
        turtle {
            do! Generate (n - 1) (v / 2.)
        }
    and GenerateRotatedSubTree n v a = 
        turtle {
            do! Turtle.Turn a
            do! GenerateSubTree n v
        }
    and GenerateBranch n v = 
        turtle {
            do! Turtle.Turn Right
            do! Turtle.Forward v
            do! GenerateSubTree n v
        }
    and Generate n v = 
        turtle {
            if n <= 0 then
                return ()
            else
                do! Turtle.Width <| (1.5 * float n)
                do! Turtle.Forward v
                do! Turtle.Turn Left
                do! GenerateSubTree n v
                do! GenerateRotatedSubTree n v Right
                do! Turtle.Turn Right
                do! Turtle.Forward v
                do! GenerateBranch n v
                do! Turtle.Turn Left
                do! GenerateSubTree n v
        }
     

