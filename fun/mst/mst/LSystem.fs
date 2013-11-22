namespace mst

module LSystem =
    type Alphabet =
        | DoNothing     // X
        | Forward       // F
        | TurnLeft      // -
        | TurnRight     // +
        | Store         // [
        | Restore       // ]

    let Angle = 25.

    let Initial = [DoNothing]

    let Rule (a : Alphabet) = 
        match a with
                     // F       -        [     [     X         ]       +         X         ]       +         F       [     +         F       X         ]       -        X 
        | DoNothing -> [Forward;TurnLeft;Store;Store;DoNothing;Restore;TurnRight;DoNothing;Restore;TurnRight;Forward;Store;TurnRight;Forward;DoNothing;Restore;TurnLeft;DoNothing]
                     // F       F
        | Forward   -> [Forward;Forward]
        | _         -> [a]

    let rec Generate =  function 
                        | 0 ->  Initial         
                        | n ->  let previous = Generate (n - 1)
                                previous |> List.collect Rule



