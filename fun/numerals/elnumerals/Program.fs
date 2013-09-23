
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

let Zero    = ZeroPad
let One     = Slot (OneBit, ZeroPad)

let rec Add (l : Natural) (r : Natural) = 
    match l,r with
    | ZeroPad           , _                 -> r
    | _                 , ZeroPad           -> l
    | Slot (ZeroBit, lt), Slot (rb, rt)     -> Slot (rb, Add lt rt)
    | Slot (lb, lt)     , Slot (ZeroBit, rt)-> Slot (lb, Add lt rt)
    | Slot (_, lt)      , Slot (_, rt)      -> Slot (ZeroBit, Add (Add lt rt) One)


let inline ( + ) l r    = Add l r

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
