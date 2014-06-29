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

namespace Protobuf.TypeProvider

open Microsoft.FSharp.Core.CompilerServices 
open Microsoft.FSharp.Quotations

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open Protobuf
open ProtoSpecification
open ProtobufParser

[<TypeProvider>]
type ProtobufTypeProvider() as this =
    inherit TypeProviderForNamespaces()

    let todo m = 
        failwith "To be implemented: %A" m

    let exeAssembly     = Assembly.GetExecutingAssembly ()
    let rootNamespace   = "Protobuf.TypeProvider"
    let baseType        = typeof<obj>
    let messageType     = typeof<ProtoMessage>
    let getMethod       = messageType.GetMethod ("Get")
    let setMethod       = messageType.GetMethod ("Set")
    let staticParams    = 
        [
            ProvidedStaticParameter ("protobufSpecification", typeof<string>)
        ]

    let providerType    = ProvidedTypeDefinition (exeAssembly, rootNamespace, "ProtobufTyped", Some baseType)

    let getFieldType (ft : FieldType) : Type =
        match ft with
        | Double        -> typeof<System.Double >
        | Float         -> typeof<System.Single >
        | Int32         -> typeof<System.Int32  >
        | Int64         -> typeof<System.Int64  >
        | UInt32        -> typeof<System.UInt32 >
        | UInt64        -> typeof<System.UInt64 >
        | SInt32        -> typeof<System.Int32  >
        | SInt64        -> typeof<System.Int64  >
        | Fixed32       -> typeof<System.UInt32 >
        | Fixed64       -> typeof<System.UInt64 >
        | SFixed32      -> typeof<System.Int32  >
        | SFixed64      -> typeof<System.Int64  >
        | Bool          -> typeof<System.Boolean>
        | String        -> typeof<System.String >
        | Bytes         -> typeof<System.Byte []>
        | UserType  _   -> todo ft

    let getDefaultValue (ft : FieldType) : obj =
        match ft with
        | Double        -> upcast 0.
        | Float         -> upcast 0.F
        | Int32         -> upcast 0
        | Int64         -> upcast 0L
        | UInt32        -> upcast 0u
        | UInt64        -> upcast 0UL
        | SInt32        -> upcast 0
        | SInt64        -> upcast 0L
        | Fixed32       -> upcast 0u
        | Fixed64       -> upcast 0UL
        | SFixed32      -> upcast 0
        | SFixed64      -> upcast 0L
        | Bool          -> upcast false
        | String        -> upcast ""
        | Bytes     _    
        | UserType  _   -> todo ft

    let getValue (v : Value) : obj =
        match v with
        | StringValue s     -> upcast s 
        | BoolValue b       -> upcast b     
        | FloatValue f      -> upcast f
        | IntValue i        -> upcast i
        | VariableValue _   -> todo v

    let appendMessage (ty : ProvidedTypeDefinition) (message : Message) = 
        let name, members = message
        let messageTy = 
            ProvidedTypeDefinition(
                name                                    , 
                baseType            = Some messageType  ,
                HideObjectMethods   = false             )

        messageTy.AddXmlDoc <| sprintf "Message %s" name

        for m in members do
            match m with
            | MemberField       field   -> 
                let modifier,fieldType,name,idx,options = field


                let picker = function
                    | FieldDefaultValue v   -> Some v
                    | _                     -> None
                let dv = 
                    match options |> List.tryPick picker with
                    | Some v    -> getValue v
                    | _         -> getDefaultValue fieldType

                let ft = getFieldType fieldType
                let gm = getMethod.MakeGenericMethod (ft)
                let sm = setMethod.MakeGenericMethod (ft)
                let fi = ProvidedProperty (
                            name    , 
                            ft      ,
                            GetterCode = fun i -> 
                                match i with
                                | [expr] ->
                                    Expr.Call (expr, gm, [Expr.Value idx; Expr.Value(dv, ft)])
                                | _ -> failwith "Unexpected input: %A" i   
                            ,
                            SetterCode = fun i -> 
                                match i with
                                | [expr; v] ->
                                    Expr.Call (expr, sm, [Expr.Value idx; v])
                                | _ -> failwith "Unexpected input: %A" i
                            )

                messageTy.AddMember fi
            | MemberEnum        _
            | MemberMessage     _
            | MemberExtend      _
            | MemberExtension   _
            | MemberGroup       _
            | MemberOption      _       -> todo m

        let ctor = ProvidedConstructor (
                    parameters  = [],
                    InvokeCode  =  
                        fun args -> <@@ Protobuf.ProtoMessage() @@>
                    )
        messageTy.AddMember ctor

        ty.AddMember messageTy

    do 
        providerType.DefineStaticParameters(
            staticParameters        =staticParams                   , 
            apply                   =(fun typeName parameterValues ->

              match parameterValues with 
              | [| :? string as protobufSpecification |] -> 
                
                let result = ParseProtobuf protobufSpecification
                
                let proto =
                    match result with
                    | ParseSuccess v    -> v
                    | ParseFailure m    -> failwithf "Parsing of protobuf specification failed: %s" m

                let ty = ProvidedTypeDefinition (
                            exeAssembly     , 
                            rootNamespace   , 
                            typeName        , 
                            Some baseType   )

                ty.AddXmlDoc "A strongly typed interface to a protobuf specification"
                
                for protoMember in proto do
                    match protoMember with
                    | ProtoMessage  message -> appendMessage    ty message
                    | ProtoExtend   _   
                    | ProtoEnum     _
                    | ProtoImport   _
                    | ProtoPackage  _
                    | ProtoOption   _       -> todo protoMember

                ty
              | _ -> failwith "unexpected parameter values")) 

    do this.AddNamespace(rootNamespace, [providerType])

