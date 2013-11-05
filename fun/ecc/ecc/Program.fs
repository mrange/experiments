// y2 = x3 + ax + b (mod p)


type Point = bigint*bigint

//type ecc(a : bigint, b : bigint, p : bigint) = 

let a = 1I 
let b = 5I
let p = 11I

(*
    let JMM y z = 108. - (815. - 1500./z)/y

    let ApplyJMM n = 
        let mutable a = 4.
        let mutable b = 4.25
        printfn "%f" a
        printfn "%f" b
        for i in 0..n do
            let x = JMM b a 
            a <- b
            b <- x
            printfn "%f" b

    ApplyJMM 80
*)

(*
    // Have to define a bigrational type as F# doesn't have it out of box
    type BigRational = bigint*bigint

    // The minus operation    
    let inline ( --- ) ((lx, ly) : BigRational) ((rx, ry) : BigRational) = 
        let x = lx * ry - rx * ly
        let y = ly * ry
        let gcd = bigint.GreatestCommonDivisor(x,y)
        if gcd.IsOne then x,y
        else (x / gcd),(y / gcd)

    // The divide operation    
    let inline ( /-/ ) ((lx, ly) : BigRational) ((rx, ry) : BigRational) = 
        let x = lx * ry
        let y = ly * rx
        let gcd = bigint.GreatestCommonDivisor(x,y)
        if gcd.IsOne then x,y
        else (x / gcd),(y / gcd)

    // Constructs a BigRational from an integer
    let br (i : bigint) : BigRational = i, 1I

    let JMM y z = (br 108I) --- ((br 815I) --- (br 1500I)/-/z)/-/y

    let ApplyJMM n = 
        let mutable a : BigRational = 4I,1I
        let mutable b : BigRational = 17I,4I
        for i in 0..n do
            let x = JMM b a 
            a <- b
            b <- x

            let t,d = b

            printfn "%s, %s" (t.ToString()) (d.ToString())
    ApplyJMM 80

*)
    

let divrem (l : bigint) (r : bigint) : bigint*bigint = 
    let rem = ref 0I 
    let div = bigint.DivRem(l, r, rem)

    div, !rem

let rec gcd (a : bigint) (b : bigint) = 
    if b = 0I then a
    else gcd b (a % b)

let rec extendedGCD (a : bigint) (b : bigint) : bigint*bigint= 
    if b.IsZero then 1I,0I
    else    let q,r = divrem a b
            let s,t = extendedGCD b r
            t, (s - q*t)

let isQuadraticResidue (z : bigint) =
    bigint.ModPow(z, (p - 1I)/2I, p) = 1I


let div2 (l : bigint) (r : bigint) = 
    l

let ( /*/ ) l r = div2 l r

let add ((px, py) : Point) ((qx, qy) : Point) : Point =  
    let th =    if (px, py) = (qx, qy) then (qy-py) /*/ (qx - px)
                else (3I*px*px + a) /*/ (2I*py)
        
    let x = (th * th - px - qx) % p
    let y = (th * (px - x) - px) % p

    x,y

let ( +*+ ) l r = add l r


#if COMPILED
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
#endif