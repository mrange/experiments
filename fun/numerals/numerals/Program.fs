
type Numeral = 
    |   Zero
    |   S of Numeral
    member x.Fold f seed = 
        match x with
        |   Zero    -> seed
        |   S n'    -> f (n'.Fold f seed) 
    member x.ToInteger () = x.Fold (fun i -> i + 1) 0
    override x.ToString () = (x.ToInteger ()).ToString ()

let Succ n  = S n
let Fold f seed (n : Numeral) = n.Fold f seed

let Add l r = Fold Succ l r
let Mul l r = Fold (Add l) Zero r

let inline ( + ) (l : Numeral) (r : Numeral)    = Add l r
let inline ( * ) (l : Numeral) (r : Numeral)    = Mul l r

let Zero    = Zero
let One     = Succ Zero
let Two     = Succ One
let Three   = Succ Two
let Four    = Succ Three
let Five    = Succ Four 
let Six     = Succ Five 
let Seven   = Succ Six  
let Eight   = Succ Seven
let Nine    = Succ Eight
                    
let TestAdd l r = 
    let result = l + r
    printf "%s + %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestMul l r = 
    let result = l * r
    printf "%s * %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())
    


[<EntryPoint>]
let main argv = 

    TestAdd Two     Zero
    TestAdd Two     One
    TestAdd Two     Two
    TestAdd Five    Seven
    
    TestAdd Zero    Two 
    TestAdd One     Two 
    TestAdd Two     Two 
    TestAdd Seven   Five

    
    TestMul Two     Zero
    TestMul Two     One
    TestMul Two     Two
    TestMul Five    Seven
    
    TestMul Zero    Two 
    TestMul One     Two 
    TestMul Two     Two 
    TestMul Seven   Five

    0
