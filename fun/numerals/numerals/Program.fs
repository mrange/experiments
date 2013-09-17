
[<StructuralEquality>]
[<StructuralComparison>]
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
let rec Until e n v = 
    if e v then v
    else Until e n (n v)
        
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
                    
let Add l r = Fold Succ l r
let Mul l r = Fold (Add l) Zero r
let Sub l r = 
    match l,r with
    | _     when l < r  -> Zero
    | _                 -> 
        let _,n = Until (fun (rem,_) -> rem = l) (fun (rem,n) -> (Succ rem, Succ n)) (r, Zero)
        n

// Not done yet
let DivMod l r = 
    match l,r with
    | _,Zero                -> Zero,Zero
    | _     when l < r      -> Zero,r
    | _                     -> 
        let _,n = Until (fun (acc,_) -> acc > l) (fun (acc,n) -> (Add acc r, Succ n)) (Zero, Zero)
        (Sub n One), Zero

let inline ( + ) (l : Numeral) (r : Numeral)    = Add l r
let inline ( - ) (l : Numeral) (r : Numeral)    = Sub l r
let inline ( * ) (l : Numeral) (r : Numeral)    = Mul l r

let TestAdd l r = 
    let result = l + r
    printf "%s + %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestSub l r = 
    let result = l - r
    printf "%s - %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestMul l r = 
    let result = l * r
    printf "%s * %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())
    
let TestDm l r = 
    let n,rem = DivMod l r
    printf "%s / %s = %s,%s\n" (l.ToString ()) (r.ToString ()) (n.ToString ()) (rem.ToString ())
    
let TestEq l r = 
    let result = l = r
    printf "%s = %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestLt l r = 
    let result = l < r
    printf "%s < %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestGt l r = 
    let result = l > r
    printf "%s > %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

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

    TestSub Zero    Two 
    TestSub One     Two 
    TestSub Two     Two 
    TestSub Seven   Five
    
    TestSub Two     Zero    
    TestSub Two     One     
    TestSub Two     Two     
    TestSub Five    Seven   
    
    TestMul Two     Zero
    TestMul Two     One
    TestMul Two     Two
    TestMul Five    Seven
    
    TestMul Zero    Two 
    TestMul One     Two 
    TestMul Two     Two 
    TestMul Seven   Five

    TestDm  Eight   Two     
    TestDm  Eight   One
    TestDm  Eight   Three
    TestDm  Eight   Four

    TestEq  Zero    Zero
    TestEq  Zero    Two
    TestEq  Two     Zero
    TestEq  Two     Two

    TestLt  Zero    Zero
    TestLt  Zero    Two
    TestLt  Two     Zero
    TestLt  Two     Two

    TestGt  Zero    Zero
    TestGt  Zero    Two
    TestGt  Two     Zero
    TestGt  Two     Two

    0
