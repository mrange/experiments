
// Define a bit and related operations
type Bit = 
    | ZeroBit 
    | OneBit

// Define a natural number and related operations
type Natural = 
    | ZeroPad
    | Slot of Bit*Natural

    override x.ToString () =
        // For debugging reasons only

        let rec ToBigInt m n s = 
            match n with
            |   ZeroPad -> s
            |   Slot (ZeroBit, n')  -> ToBigInt (2I*m) n' s
            |   Slot (OneBit, n')   -> ToBigInt (2I*m) n' (s + m)
            
        let bi = ToBigInt 1I x 0I
        bi.ToString ()
    
    static member New b n = 
        match b,n with
        | ZeroBit, ZeroPad  -> failwith "Natural type invariant requires OneBit to precede ZeroPad"
        | _                 -> Slot (b, n)

let Zero    = ZeroPad
let One     = Slot (OneBit, ZeroPad)

// An active pattern in order to simplify pattern matching
let (|IsZero|IsOne|IsOther|) (x : Natural) = 
    match x with
    |   ZeroPad                 -> IsZero
    |   Slot (OneBit, ZeroPad)  -> IsOne
    |   _                       -> IsOther

let rec Add (l : Natural) (r : Natural) = 
    match l,r with
    | IsZero            , _                 -> r
    | _                 , IsZero            -> l
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

let rec Multiply (l : Natural) (r : Natural) = 
    match l,r with
    | IsZero            , _                 -> Zero
    | _                 , IsZero            -> Zero
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

let Two     = One + One
let Three   = One + Two  
let Four    = One + Three
let Five    = One + Four 
let Six     = One + Five 
let Seven   = One + Six  
let Eight   = One + Seven
let Nine    = One + Eight

let print (v : Natural) = 
    printf "%s = %A\n" (v.ToString ()) v
 
let TestAdd (l : Natural) (r : Natural) = 
    let result = l + r
    printf "%s + %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())
 
let TestMul (l : Natural) (r : Natural) = 
    let result = l * r
    printf "%s * %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

let TestPow (l : Natural) (r : Natural) = 
    let result = l ^^^ r
    printf "%s ^^^ %s = %s\n" (l.ToString ()) (r.ToString ()) (result.ToString ())

[<EntryPoint>]
let main argv = 

    printf "Number 0..9\n"

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

    printf "Testing operators\n"

    TestAdd Six     Zero
    TestAdd Six     Nine
    TestAdd Two     Two
    TestAdd Seven   Five

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

    0
