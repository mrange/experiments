
// Define a bit and related operations
type Bit = 
    | ZeroBit 
    | OneBit
    // And
    static member inline ( * ) (l,r) = 
        match l,r with
        |   OneBit,OneBit   -> OneBit
        |   _               -> ZeroBit
    // Or
    static member inline ( + ) (l,r) = 
        match l,r with
        |   ZeroBit,ZeroBit -> ZeroBit
        |   _               -> OneBit
    // Xor
    static member inline ( ^^ ) (l,r) = 
        match l,r with
        |   ZeroBit,OneBit  -> OneBit
        |   OneBit,ZeroBit  -> OneBit
        |   _               -> ZeroBit

// HalfAdd produces a carry bit and the sum from two bits
let inline HalfAdd (l : Bit) (r : Bit) =
    (l * r),(l ^^ r)

// FullAdd produces a carry bit and the sum from a carry and two bits
let inline FullAdd (carry : Bit) (l : Bit) (r : Bit) =
    let c,ha = HalfAdd l r
    (c + carry * (l + r)), (ha ^^ carry)

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

// Expands a Natural into a tuple
let inline (|Expand|) (v : Natural) = 
    match v with 
    |   ZeroPad         -> ZeroBit, ZeroPad
    |   Slot (bit,tail) -> bit, tail

let rec AddImpl (carry: Bit) (l : Natural) (r : Natural) = 
    match carry,l,r with
    | ZeroBit   , ZeroPad           , ZeroPad           -> ZeroPad
    | OneBit    , ZeroPad           , ZeroPad           -> Slot (OneBit, ZeroPad)
    | ZeroBit   , _                 , ZeroPad           -> l    // Short-cut
    | ZeroBit   , ZeroPad           , _                 -> r    // Short-cut
    | carry     , Expand (lb, ln)   , Expand (rb, rn)   -> 
        let c,fa = FullAdd carry lb rb
        Slot (fa, AddImpl c ln rn)

let Add (l : Natural) (r : Natural) = AddImpl ZeroBit l r

let inline ( + ) l r    = Add l r

let Zero    = ZeroPad
let One     = Slot (OneBit, ZeroPad)
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
 
[<EntryPoint>]
let main argv = 

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

    print (Six + Nine)
    print (Two + Two)
    print (Seven + Five)

    0
