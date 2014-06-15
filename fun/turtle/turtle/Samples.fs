namespace TurtlePower

open Turtle

module SimpleBox = 

    let Generate v time = 
        turtle {
            do! Forward v
            do! Forward v
            do! Turn 90.F
            do! Forward v
            do! Turn 90.F
            do! Forward v
            do! Turn 90.F
            do! Forward v
        }

module Box = 

    let Generate v time = 
        turtle {
            do! Forward v
            do! Forward v
            for i in 0..2 do
                do! Turn 90.F
                do! Forward v
        }
        
module RecursiveBox = 

    let Generate v time = 
        turtle {
//            let d = v + 50.0F * (sin <| 20.F * Deg2Rad * time)
            let d = v
            do! Forward v
            do! Forward v
            for i in 0..200 do
                do! Turn -88.F
                let divisor = pown 1.02F i
                do! Forward <| d / divisor
        }


module SimpleTreeFractal = 

    let rec Generate n v time = 
        turtle {
            if n <= 0 then
                return ()
            else
                do! Width 6.0F

                // Step 1
                do! Forward v

                // Step 2
                do! Turn 30.0F
                do! Generate (n - 1) (v * 0.75F) time
                do! Turn -30.0F

                // Step 3
                do! Turn -30.0F
                do! Generate (n - 1) (v * 0.75F) time
                do! Turn 30.0F

                do! Forward -v
        }

module TreeFractal = 

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
                let c = 
                    match n with
                    | _ when n < 3  -> Lime
                    | _ when n < 6  -> LimeGreen
                    | _             -> Brown

                do! Color c
                do! Width <| float32 n

                let turn = 30.F
                //let turn = (3.0F * time) % 180.0F

                do! Forward v
                do! RunAndReturn <| GenerateBranch n (v * 0.75F) time turn
                do! GenerateBranch n (v * 0.75F) time -turn
        }

module WavingTreeFractal = 

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
                let turn = min 20.F <| 1.F + time
                let c = 
                    match n with
                    | _ when n < 3  -> Lime
                    | _ when n < 6  -> LimeGreen
                    | _             -> Brown

                do! Color c
                do! Turn <| 10.F * (sin <| 20.F * Deg2Rad * time)
                do! Width <| float32 n
                do! Forward v
                do! RunAndReturn <| GenerateBranch n (v * LeftScaling) time turn
                do! GenerateBranch n (v * RightScaling) time -turn
        }


module OptimizedTreeFractal =

    let LeftScaling     = 1.F / sqrt 2.F
    let RightScaling    = 1.1F * LeftScaling
    
    let GenerateFlower v = 
        Color MediumVioletRed
        >>+ Forward v
        >>+ Turn 90.F
        >>+ Forward (v / 2.F)
        >>+ Turn -120.F
        >>+ Forward v
        >>+ Turn -120.F
        >>+ Forward v
        >>+ Turn -120.F
        >>+ Forward (v / 2.F)

    let rec GenerateBranch n v time a = 
        Turn a
        >>+ Generate (n - 1) v time

    and Generate n v time = 
        if n <= 0 then
            GenerateFlower v
        else
            let turn = min 20.F <| 1.F + time
            let c = 
                match n with
                | _ when n < 3  -> Lime
                | _ when n < 6  -> LimeGreen
                | _             -> Brown
            let turn = 10.F * (sin <| 20.F * Deg2Rad * time)

            Color c
            >>+ Turn turn
            >>+ Width (float32 n)
            >>+ Forward v
            >>+ RunAndReturn (GenerateBranch n (v * LeftScaling) time turn)
            >>+ GenerateBranch n (v * RightScaling) time -turn

module OtherTreeFractal =

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
                let c = 
                    match n with
                    | _ when n < 3  -> Lime
                    | _ when n < 6  -> LimeGreen
                    | _             -> Brown

                do! Color c
                do! Width <| float32 n

                do! Forward <| v
                do! RunAndReturn <| GenerateBranch n (v * 0.7F) time -30.F

                do! Forward <| v / 2.0F
                do! RunAndReturn <| GenerateBranch n (v * 0.5F) time 25.F

                do! Forward <| v
                do! RunAndReturn <| GenerateBranch n (v * 0.5F) time 25.F
        }

