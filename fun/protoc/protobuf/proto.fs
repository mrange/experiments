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


namespace Protobuf

open System.Collections.Generic

open ProtoSpecification

type ProtoWireValue =
    | Variant       of uint64
    | ZigZag        of int64
    | Fixed64       of uint64
    | Embedded      of int
    | BeginGroup    
    | EndGroup      
    | Fixed32       of uint32

type ProtoWriter() = 
    let bytes = ResizeArray<byte>()

    member x.Write (id : int) (v : ProtoWireValue)  = ()

type ProtoReader(bytes : byte[]) = 
    let mutable pos = 0

    member x.Read () : ProtoWireValue option = None

    member x.Advance (i : int)  = 
        pos <- pos + i

    member x.SkipGroup ()  = 
        ()

type ProtoMessage() = 
    let dictionary  = Dictionary<int,obj>()

    member x.Get<'T> (id : int) (defaultValue : 'T)    = 
        let mutable value : obj = null
        if dictionary.TryGetValue(id, &value) then
            match value with 
            | :? 'T as v    -> v
            | _             -> defaultValue
        else
            defaultValue

    member x.Set (id : int) (value : 'T)  = 
        dictionary.[id] <- value




