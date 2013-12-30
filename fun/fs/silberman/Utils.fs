namespace silberman

open SharpDX

open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Windows.Forms

[<AutoOpen>]
module internal Utils =
    
    let inline (|IsNaN|IsPositiveInfinity|IsNegativeInfinity|IsNegative|IsPositive|) (v : float32) = 
        if      Single.IsNaN                v then IsNaN
        elif    Single.IsPositiveInfinity   v then IsPositiveInfinity
        elif    Single.IsNegativeInfinity   v then IsNegativeInfinity
        elif    v < 0.F                       then IsNegative
        else                                       IsPositive

    let inline DefaultOf<'T> = Unchecked.defaultof<'T>
    let inline RefOf<'T> = ref DefaultOf<'T>

    let inline Zero<'T when 'T : struct> = DefaultOf<'T>

    let inline Clamp (v : float32) =
        match v with
        | IsPositiveInfinity    -> v
        | IsPositive            -> v
        | _                     -> 0.F

    let Log             (message  : string)= printfn "Information : %s" message
    let LogWarning      (message  : string)= printfn "Warning     : %s" message
    let LogError        (message  : string)= printfn "Error       : %s" message
    let LogException    (exn      : exn)   = printfn "Exception   : %s" exn.Message

    let Deg2Rad = float32 Math.PI/180.F
    let Rad2Deg = 1.F / Deg2Rad

    let Vector2 x y = Vector2(x,y)

    let Normalize (v : Vector2) = v.Normalize(); v

    let TryDispose (disposabe : IDisposable) = 
        try
            if disposabe <> null then disposabe.Dispose ()
        with
        | exn -> printfn "Caught exception: %A" exn

    let TryRun (action : unit -> unit) = 
        try
            action()
        with
        | exn -> printfn "Caught exception: %A" exn


    type Disposable(action : unit->unit) = 
        interface IDisposable with
            member x.Dispose() = TryRun action

    let OnExit action : IDisposable = upcast new Disposable(action)

    let AsNullable value = Nullable<_>(value)

    let CompareAndExchange<'T when 'T : not struct> (f : 'T -> 'T) (valueReference : 'T ref) = 
        while let value = !valueReference in not (Object.ReferenceEquals (Interlocked.CompareExchange (valueReference, f value, value), value)) do
            ()

    let DispatchAction (control : Control) (action : unit->unit) = 
        let a = Action action
        ignore <| control.BeginInvoke(a)

    let DefaultTo (optional : 'T option) (defaultValue : 'T) = 
        match optional with
        | Some v    -> v
        | _         -> defaultValue

    let CastTo<'T> (o : obj) (defaultValue : 'T) = 
        match o with
        | :? 'T as v    -> v
        | _             -> defaultValue
        
    let As<'T> (o : obj) = 
        match o with
        | :? 'T as v    -> Some v
        | _             -> None
        
    let Is<'T> (o : obj) = 
        match o with
        | :? 'T         -> true
        | _             -> false
        

    let CombineDisposable (l : #IDisposable) (r : #IDisposable) = 
        OnExit <| fun () -> 
                    TryDispose l
                    r.Dispose ()

    // TODO: Scrap these and replace with extension methods?
    // Extension methods seems easier to maintain since function application has high precence leads to less parantese

    let inline ( <??> ) optional defaultValue = DefaultTo optional defaultValue
    let inline ( <???> ) o defaultValue = CastTo o defaultValue

    type IDisposable with 
        member x.Combine (o : IDisposable) = CombineDisposable x o            

    type Matrix3x2 with 
        member x.Multiply (o : Matrix3x2) = Matrix3x2.Multiply(x,o) 
        member x.TransformPoint (p : Vector2) = Matrix3x2.TransformPoint(x,p)

    type Type with 
        member x.Ancestors () = 
            let t = ref x
            seq {
                while !t <> null do
                    yield !t
                    t := (!t).BaseType
            }

    type RectangleF with
        member x.Union (other : RectangleF) =
            if x.IsEmpty then other
            elif other.IsEmpty then x
            else 
                let left    = min x.Left    other.Left
                let top     = min x.Top     other.Top
                let right   = max x.Right   other.Right
                let bottom  = max x.Bottom  other.Bottom
                RectangleF (left, top, right - left, bottom - top)

    type Object with
        member x.CastTo (defaultValue : 'T) = x <???> defaultValue
        member x.As<'T> () = As<'T> x
        member x.Is<'T> () = Is<'T> x

    type IDictionary<'TKey, 'TValue> with
        member x.Lookup (key : 'TKey) (defaultValue : 'TValue) =  
                    let v = RefOf<'TValue>
                    if x.TryGetValue(key, v) then !v
                    else defaultValue

        member x.Find (key : 'TKey) : 'TValue option =  
                    let v = RefOf<'TValue>
                    if x.TryGetValue(key, v) then Some !v
                    else None
                                                

