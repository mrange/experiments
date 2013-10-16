
open System

// Math from scratch, part two: zero and one

// Define a bit and related operations
type Bit = 
    | ZeroBit 
    | OneBit

// Define a natural number and related operations
[<StructuralEquality>]
// Requires us to implement IComparable but then all comparison operators works
[<CustomComparison>]    
type Natural = 
    | ZeroPad
    | Slot of Bit*Natural

    // Math from scratch, part six: comparisons
    interface IComparable<Natural> with 
        member x.CompareTo other = Natural.CompareTo x other

    // Math from scratch, part six: comparisons
    interface IComparable with 
        member x.CompareTo other = 
            match other with
            | :? Natural as n   -> Natural.CompareTo x n
            | _                 -> 0
     
    override x.ToString () =
        // For debugging reasons only

        let rec ToBigInt m n s = 
            match n with
            |   ZeroPad -> s
            |   Slot (ZeroBit, n')  -> ToBigInt (2I*m) n' s
            |   Slot (OneBit, n')   -> ToBigInt (2I*m) n' (s + m)
            
        let bi = ToBigInt 1I x 0I
        bi.ToString ()
    
    // Math from scratch, part six: comparisons
    static member CompareTo (l : Natural) (r : Natural) = 
        match l,r with
        |   ZeroPad         , ZeroPad       -> 0
        |   _               , ZeroPad       -> 1
        |   ZeroPad         , _             -> -1
        |   Slot (lb, lt)   , Slot (rb, rt) ->
            let compare = Natural.CompareTo lt rt

            match compare, lb, rb with 
            | -1, _         , _         -> -1
            |  1, _         , _         ->  1
            |  _, OneBit    , ZeroBit   ->  1
            |  _, ZeroBit   , OneBit    ->  -1
            |  _                        ->  0


    static member New b n = 
        match b,n with
        | ZeroBit, ZeroPad  -> ZeroPad
        | _                 -> Slot (b, n)

// Math from scratch, part two: zero and one
let Zero    = ZeroPad
let One     = Slot (OneBit, ZeroPad)

// An active pattern in order to simplify pattern matching
let (|IsZero|IsOne|IsOther|) (x : Natural) = 
    match x with
    |   ZeroPad                 -> IsZero
    |   Slot (OneBit, ZeroPad)  -> IsOne
    |   _                       -> IsOther


// Math from scratch, part three: natural addition
let rec Add (l : Natural) (r : Natural) = 
    match l,r with
    | _                 , IsZero            -> l
    | IsZero            , _                 -> r
    | Slot (ZeroBit, lt), Slot (rb, rt)     -> Natural.New 
                                                <| rb 
                                                <| Add lt rt
    | Slot (lb, lt)     , Slot (ZeroBit, rt)-> Natural.New 
                                                <| lb
                                                <| Add lt rt
    | Slot (_, lt)      , Slot (_, rt)      -> Natural.New
                                                <| ZeroBit
                                                <| Add (Add lt rt) One

let inline ( + ) l r    = Add l r

// Math from scratch, part four: natural multiplication
let rec Multiply (l : Natural) (r : Natural) = 
    match l,r with
    | _                 , IsZero            -> Zero
    | IsZero            , _                 -> Zero
    | IsOne             , _                 -> r
    | _                 , IsOne             -> l
    | Slot (ZeroBit, lt), _                 -> Natural.New
                                                <| ZeroBit
                                                <| Multiply lt r
    | _                 , Slot (ZeroBit, rt)-> Natural.New
                                                <| ZeroBit
                                                <| Multiply l rt
    | Slot _            , Slot (_, rt)      -> let x = Natural.New
                                                        <| ZeroBit
                                                        <| Multiply l rt
                                               x + l

let inline ( * ) l r    = Multiply l r

let rec Power (l : Natural) (r : Natural) = 
    match l,r with
    | _                 , IsZero            -> One  // 0 ^ 0 is defined as 1
    | IsZero            , _                 -> Zero
    | IsOne             , _                 -> One
    | _                 , IsOne             -> l
    | _                 , Slot (rb, rt)     -> let x = Power l rt
                                               let xx = x * x
                                               if rb = OneBit then xx * l
                                               else xx 

let inline ( ^^^ ) l r    = Power l r

// Math from scratch, part five: natural subtraction
let rec Sub (l : Natural) (r : Natural) = 
    match l,r with
    | _                 , IsZero            -> l
    | IsZero            , _                 -> failwith "Sub fails as the result would not be a natural number"
    | Slot (lb, lt)     , Slot (ZeroBit, rt)-> Natural.New 
                                                <| lb
                                                <| Sub lt rt
    | Slot (OneBit, lt) , Slot (OneBit, rt) -> Natural.New 
                                                <| ZeroBit
                                                <| Sub lt rt
    | Slot (_, lt)      , Slot (_, rt)      -> Natural.New
                                                <| OneBit
                                                <| Sub (Sub lt rt) One

let inline ( - ) l r    = Sub l r

// Math from scratch, part seven: division and remainder
let rec DivMod (l : Natural) (r : Natural) = 
    match l,r with
    | _                 , IsZero            -> failwith "DivMod fails as the divider is Zero"
    | IsZero            , _                 -> Zero,Zero
    | _                 , IsOne             -> l,Zero
    | _                 , _     when l = r  -> One,Zero
    | _                 , _     when l < r  -> Zero,l
    | Slot (lb, lt)     , _                 -> let q,rem    = DivMod lt r
                                               let q'       = Natural.New ZeroBit   q
                                               let rem'     = Natural.New lb        rem

                                               if rem' < r then q',rem'
                                               else (q' + One), (rem' - r)

let inline ( /% ) l r   = DivMod l r

let Two     = One + One
let Three   = One + Two  
let Four    = One + Three
let Five    = One + Four 
let Six     = One + Five 
let Seven   = One + Six  
let Eight   = One + Seven
let Nine    = One + Eight

let print (v : Natural) = 
    printfn "%s = %A" (v.ToString ()) v
 
let TestAdd (l : Natural) (r : Natural) = 
    let result = l + r
    printfn "%s + %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())
 
let TestSub (l : Natural) (r : Natural) = 
    let result = l - r
    printfn "%s - %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())
 
let TestMul (l : Natural) (r : Natural) = 
    let result = l * r
    printfn "%s * %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestPow (l : Natural) (r : Natural) = 
    let result = l ^^^ r
    printfn "%s ^^^ %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestEq l r = 
    let result = l = r
    printfn "%s = %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestNeq l r = 
    let result = l <> r
    printfn "%s <> %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestLt l r = 
    let result = l < r
    printfn "%s < %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestGt l r = 
    let result = l > r
    printfn "%s > %s = %s" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestDivmod (l : Natural) (r : Natural) = 
    let q,rem = l /% r
    printfn "%s / %s = %s,%s" (l.ToString ()) (r.ToString ()) (q.ToString ()) (rem.ToString ())

[<EntryPoint>]
let main argv = 

    printfn "Number 0..9"

    print Zero
    print One   
    print Two   
    print Three 
    print Four  
    print Five  
    print Six   
    print Seven 
    print Eight 
    print Nine  

    printfn "Testing operators"

    printfn "Math from scratch, part three: natural addition"
    TestAdd Six     Zero
    TestAdd Six     Nine
    TestAdd Two     Two
    TestAdd Seven   Five

    printfn "Math from scratch, part four: natural multiplication"
    TestMul Six     Zero
    TestMul Zero    Two
    TestMul Six     One
    TestMul One     Two
    TestMul Two     Two
    TestMul Two     Three
    TestMul Three   Two
    TestMul Six     Nine
    TestMul Two     Two
    TestMul Seven   Five

    TestPow Zero    Zero
    TestPow Zero    One
    TestPow Zero    One
    TestPow One     One
    TestPow Two     Two
    TestPow Two     Eight
    TestPow Five    Three
    TestPow Four    Three

    printfn "Math from scratch, part five: natural subtraction"
    TestSub Six     Zero
    TestSub Six     Three
    TestSub Two     Two
    TestSub Seven   Five

    printfn "Math from scratch, part six: comparisons"
    TestEq  Zero     Zero
    TestEq  Two      Two
    TestEq  One      Two
    TestEq  Two      One
    TestEq  Five     (Two + Three)

    TestNeq Zero     Zero
    TestNeq Two      Two
    TestNeq One      Two
    TestNeq Two      One
    TestNeq Five     (Two + Three)

    TestLt  Zero     Zero
    TestLt  Two      Two
    TestLt  One      Two
    TestLt  Two      One
    TestLt  Five     (Four + Three)
    TestLt  Seven    Nine
    TestLt  Nine     Seven

    TestGt  Zero     Zero
    TestGt  Two      Two
    TestGt  One      Two
    TestGt  Two      One
    TestGt  Five     (Four + Three)
    TestGt  Seven    Nine
    TestGt  Nine     Seven

    printfn "Math from scratch, part seven: division and remainder"

    TestDivmod  One             One
    TestDivmod  Two             One
    TestDivmod  Two             Three
    TestDivmod  Nine            Three
    TestDivmod  (Nine * Nine)   Nine
    TestDivmod  (Six * Seven)   Five

    0
