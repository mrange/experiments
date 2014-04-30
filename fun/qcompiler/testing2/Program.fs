
let f (x : int -> int -> int) = x 2 3
let g (x : int -> int -> int) = x 2


[<EntryPoint>]
let main argv = 
    let ff = f (fun x y -> x + y) 
    let gg = g (fun x y -> x + y) 



    printfn "ff:%A, gg:%A" ff gg
    0 // return an integer exit code
