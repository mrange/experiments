
let f (x : int -> int -> int) = x 2 3
let g (x : int -> int -> int) = x 2
let z (x : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int) = x 1 2 3 4 5 6 7 8 9
let q (x : int -> int -> int -> int -> int -> int -> int) = x 1 2 3 4 5 6

let w () = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20
let u () = 1,2,3,4,5,6,7,8


[<EntryPoint>]
let main argv = 
    let ff = f (fun x y -> x + y) 
    let gg = g (fun x y -> x + y) 
    let zz = z (fun p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 -> 0)
    let qq = q (fun p1 p2 p3 p4 p5 p6 -> 0)

    printfn "ff:%A, gg:%A, zz:%A, ww%A" ff gg zz <| w ()
    0
