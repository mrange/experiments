// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
// If you cannot locate the  Microsoft Public License, please send an email to
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

namespace GravitySucks

exception TodoException

[<AutoOpen>]
module Utils =
    
    open SharpDX
    open System

    let Deg2Rad = float32 Math.PI/180.F
    let Rad2Deg = 1.F / Deg2Rad

    let inline DefaultOf<'T> = Unchecked.defaultof<'T>

    let inline V2 x y = Vector2 (x,y)
    let inline ( <*> ) (l : Matrix3x2) (r : Matrix3x2) = Matrix3x2.Multiply (l,r)
    let inline Normalize (v : Vector2) = v.Normalize (); v

    let TryRun (a : unit -> unit) = 
        try
            a ()
        with
        | e -> printfn "Caught exception: %A" e

    let TryDispose (d : #IDisposable) = 
        if d <> null then
            try
                d.Dispose ()
            with
            | e -> printfn "Caught exception: %A" e

    let TryDisposeList (ds : seq<#IDisposable>) =
        for d in ds do
            TryDispose d
        

    type Disposer (action : unit->unit) = 
    
        interface IDisposable with
            member x.Dispose () = TryRun action

    let OnExit a = new Disposer (a)