
open System
open System.Collections.Generic
open System.Diagnostics
open System.Linq


type Range =
    struct
        val mutable ``begin``   : int32        
        val mutable ``end``     : int32        

        static member Empty = Range (0,0)

        member x.Count      = x.End - x.Begin

        member x.Begin      = x.``begin``
        member x.End        = x.``end``

        member x.First      = x.Begin
        member x.Last       = Debug.Assert (x.Count > 0)
                              x.End - 1

        member x.IsEmpty    = x.Count = 0

        member x.IsInside s         = s >= x.Begin && s < x.End
        member x.IsOutside s        = not <| x.IsInside s
        member x.IsStrictInside s   = s >= (x.Begin + 1) && (s + 1) < x.End
        member x.IsFirst  s         = x.Count > 0 && s = x.Begin
        member x.IsLast   s         = x.Count > 0 && s = x.Last
        member x.IsNextToFirst s    = x.First - 1 = s
        member x.IsNextToLast s     = x.End = s

        member x.Merge (y : Range)  = 
                let min = min x.Begin y.Begin
                let max = max x.End y.End
                Range (min, max)

        member x.IsOverlapping (y : Range) =
                if x.IsEmpty || y.IsEmpty then false
                else
                    x.IsInside y.Begin || x.IsInside y.Last


        member x.ReserveFirst s = 
                if x.IsFirst s then 
                    x.``begin`` <- x.Begin + 1; 
                    true
                else 
                    false

        member x.ReserveLast s = 
                if x.IsLast s then 
                    x.``end`` <- x.End - 1; 
                    true
                else 
                    false

        member x.ReserveSplit s = 
                if x.IsStrictInside s then 
                    let r1 = Range (x.Begin, s)
                    let r2 = Range (s + 1, x.End)
                    Some (r1,r2)
                else 
                    None

        member x.ReleaseNextToFirst s = 
                if x.IsNextToFirst s then 
                    x.``begin`` <- x.Begin - 1; 
                    true
                else 
                    false

        member x.ReleaseNextToLast s = 
                if x.IsNextToLast s then 
                    x.``end`` <- x.End + 1; 
                    true
                else 
                    false

        new (b, e)          = { ``begin`` = b; ``end`` = e }
    end


module Details = 

    type RangeTreeResult = 
        | Failure of string
        | Success
        | Success_EmptySubTree 
        | Success_NewSubTree    of RangeTree
    and RangeTreeJunction =
        {
            mutable Range   : Range
            mutable Left    : RangeTree
            mutable Right   : RangeTree
        }

        static member New rng l r = { Range = rng; Left = l; Right = r }

    and RangeTree =
        | Junction  of RangeTreeJunction
        | Leaf      of Range

    module Internal = 

        let inline (|SplitReserved|_|) (s : int32) (rt : RangeTree) = 
            match rt with
            | Leaf r    -> r.ReserveSplit s
            | _         -> None

        let inline (|ExpandJunction|_|) (rt : RangeTree) = 
            match rt with
            | Leaf r        -> None
            | Junction j    -> Some <| (j.Range,j.Left,j.Right)

        let inline GetRange (rt : RangeTree) = 
            match rt with
            | Junction j    -> j.Range
            | Leaf r        -> r

        let inline UpdateRange (rt : RangeTree) =
            match rt with
            | Junction j    -> j.Range <- (j.Left |> GetRange).Merge (j.Right |> GetRange)
            | Leaf _        -> ()

        let inline MergeJunction (rt : RangeTree) =
            match rt with
            | ExpandJunction (_, Leaf lr, (Leaf rr)) when lr.IsOverlapping rr   -> 
                Success_NewSubTree <| (Leaf <| lr.Merge rr)
            | _ -> Success
        

        let rec ReserveSlotImpl (s : int32) (rt : RangeTree)  = 
            match rt with
            | Junction j    when j.Range.IsOutside s->  Failure "Can't reserve slot as it's outside the range"
            | Junction j                            ->  
                let r       = j.Left |> GetRange
                let useLeft = r.IsInside s
                let c, oc   = if useLeft then j.Left, j.Right else j.Right, j.Left

                let result = ReserveSlotImpl s c 
                match result with
                | Success_EmptySubTree  -> Success_NewSubTree oc
                | Success_NewSubTree crt-> if useLeft then j.Left <- crt else j.Right <- crt
                                           UpdateRange rt
                                           Success
                | Success               -> UpdateRange rt
                                           result
                | Failure _             -> result
            
            | Leaf r        when r.IsOutside s      ->  Failure "Can't reserve slot as it's outside the range"
            | Leaf r when r.ReserveFirst s          ->  if r.IsEmpty then Success_EmptySubTree else Success
            | Leaf r when r.ReserveLast s           ->  if r.IsEmpty then Success_EmptySubTree else Success
            | SplitReserved s (r1,r2)               ->  Success_NewSubTree <| Junction (RangeTreeJunction.New (r1.Merge r2) (Leaf r1) (Leaf r2))
            | _                                     ->  failwith "Failure due to programming error"


        let rec ReleaseSlotImpl (s : int32) (rt : RangeTree)  = 
            match rt with
            | Leaf r        when r.IsInside s           ->  Failure "Can't release slot, it's already released"
            | Leaf r        when r.IsEmpty              ->  Success_NewSubTree (Leaf <| Range (s,s + 1))
            | Leaf r        when r.ReleaseNextToFirst s ->  Success
            | Leaf r        when r.ReleaseNextToLast s  ->  Success
            | Junction j    when j.Range.IsInside s ->  
                let r       = j.Left |> GetRange
                let useLeft = r.IsInside s
                let c, oc   = if useLeft then j.Left, j.Right else j.Right, j.Left

                let result  = ReleaseSlotImpl s c

                match result with
                | Success_EmptySubTree  -> Success_NewSubTree oc
                | Success_NewSubTree crt-> if useLeft then j.Left <- crt else j.Right <- crt
                                           MergeJunction rt
                | Success               -> UpdateRange rt
                                           MergeJunction rt
                | Failure _             -> result
            | _                                     ->  
                let sr  = Range(s, s + 1)
                let r   = rt |> GetRange
                let mr  = sr.Merge r
                let sl  = Leaf sr
                let j   =
                    if s < r.Begin then
                        RangeTreeJunction.New mr sl rt
                    else
                        RangeTreeJunction.New mr rt sl
                Success_NewSubTree <| Junction j

    let rec Statistics (rt : RangeTree) =
        match rt with
        | Leaf _    -> 1,1
        | Junction j-> 
            let ld, lc  = Statistics j.Left
            let rd, rc  = Statistics j.Right
            let d       = (max ld rd) + 1
            let c       = lc + rc + 1
            d,c

    let rec FillSet (set : HashSet<int32>) (rt : RangeTree) =
        match rt with
        | Leaf r when r.IsEmpty -> ()
        | Leaf r                ->
            for i in r.Begin..r.Last do
                ignore <| set.Add (i)
        | Junction j            ->
            FillSet set j.Left
            FillSet set j.Right

open Details

type CompactSet(from : int32, count : int32) =

    let emptyTree   = Leaf <| Range.Empty
    let range       = Range (from, from + count)
    let mutable rt  = Leaf range

    let UpdateTree (rtr : RangeTreeResult) =
        match rtr with
        | Failure _             -> false
        | Success               -> true
        | Success_EmptySubTree  -> rt <- emptyTree
                                   true
        | Success_NewSubTree t  -> rt <- t
                                   true

    member x.ReserveSlot (s : int32) = 
        if range.IsInside s then 
            UpdateTree <| Internal.ReserveSlotImpl s rt
        else false

    member x.ReleaseSlot (s : int32) = 
        if range.IsInside s then 
            UpdateTree <| Internal.ReleaseSlotImpl s rt
        else 
            false

    member x.Statistics = rt |> Details.Statistics

    member x.AsSet ()   = let set = HashSet<int32> ()
                          rt |> Details.FillSet set
                          set

let mutable Errors      = 0

let Error (msg : string)= 
    Errors <- Errors + 1
    printfn "Error detected: %s" msg

let Info (msg : string)= 
    printfn "Info %s" msg

let TestCase () = 
    let random      = Random (19740531)

    let from        = 1000
    let count       = 1000
    let overshoot   = 10
    let tests       = 1000

    let oracle      = new HashSet<int32>([for i in from..(from + count - 1) -> i])
    let set         = CompactSet(from, count)

    let reserve (s : int32) = oracle.Remove s
    let release (s : int32) = oracle.Add s

    for i in 1..tests do
        let test                = random.Next (from - overshoot, from + count + overshoot)
        let a, oracle_a, set_a  = 
            if random.Next(2) = 0 then 
                "reserve", reserve, set.ReserveSlot
            else
                "release", release, set.ReleaseSlot

        let oracle_r    = oracle_a test
        let set_r       = set_a test
        if oracle_r <> set_r then
            Error <| sprintf "Oracle and set doesn't agree on result when doing a %s on %d" a test


    let d,c             = set.Statistics

    Info <| sprintf "After running %d test cases the set depth is %d and the node count is %d" tests d c

    let oracle_set      = oracle
    let set_set         = set.AsSet ()

    let missing         = oracle_set.Except(set_set).ToArray()
    let extra           = set_set.Except(oracle_set).ToArray()

    if missing.Length > 0 then
        Error <| sprintf "Some items are missing in the result set"

    if extra.Length > 0 then
        Error <| sprintf "Too many items in the result set"



    ()

[<EntryPoint>]
let main argv = 
    printfn "Running test cases"

    TestCase ()

    if Errors > 0 then 
        printfn "%d errors detected!" Errors
        101
    else
        printfn "All tests passed"
        0
