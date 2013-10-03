
[<StructuralEquality>]
[<StructuralComparison>]
type Numeral = 
    |   Zero
    |   Succ of Numeral
    override x.ToString () =
        let rec Length (n : Numeral) = 
            match n with
            |   Zero    -> 0
            |   Succ p  -> 1 + Length p
        (Length x).ToString ()

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
                    
let rec Add l r = 
    match r with
    |   Zero        -> l
    |   Succ p      -> Add (Succ l) p

let inline ( + ) (l : Numeral) (r : Numeral)    = Add l r

let rec Sub l r = 
    match l,r with
    |   _       , Zero      -> l
    |   Zero    , _         -> failwith "Sub fails as the result would not be a natural number"
    |   Succ l' , Succ r'   -> Sub l' r'

let inline ( - ) (l : Numeral) (r : Numeral)    = Sub l r

let rec Mul l r = 
    match r with
    |   Zero        -> Zero
    |   Succ p      -> l + (Mul l p)

let inline ( * ) (l : Numeral) (r : Numeral)    = Mul l r

let rec Pow l r = 
    match r with
    |   Zero        -> One
    |   Succ p      -> l * (Pow l p)

let inline ( ^^^ ) (l : Numeral) (r : Numeral)    = Pow l r

let TestAdd l r = 
    let result = l + r
    printf "%s + %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestSub l r = 
    let result = l - r
    printf "%s - %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestMul l r = 
    let result = l * r
    printf "%s * %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())
    
let TestPow l r = 
    let result = l ^^^ r
    printf "%s ^^^ %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())
    
[<EntryPoint>]
let main argv = 

    TestAdd Two     Zero
    TestAdd Two     One
    TestAdd Two     Two
    TestAdd Five    Seven
    
    TestSub Two     Zero
    TestSub Two     One
    TestSub Two     Two
    TestSub Seven   Five
    
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

    TestPow Zero    Two 
    TestPow One     Two 
    TestPow Two     Zero
    TestPow Two     Two
    TestPow Four    Three

    0
