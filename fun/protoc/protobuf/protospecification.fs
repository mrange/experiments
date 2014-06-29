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

module ProtoSpecification = 

    type Value          =
        | StringValue   of string
        | BoolValue     of bool
        | FloatValue    of float
        | IntValue      of int
        | VariableValue of string

    type Modifier       =
        | Required
        | Optional
        | Repeated

    type UserType       = bool*string list

    type FieldType      =
        | Double
        | Float
        | Int32
        | Int64
        | UInt32
        | UInt64
        | SInt32
        | SInt64
        | Fixed32
        | Fixed64
        | SFixed32
        | SFixed64
        | Bool
        | String
        | Bytes
        | UserType  of UserType

    type Option         = string list*Value

    type FieldOption    =
        | FieldDefaultValue of Value
        | FieldValue        of Option

    type Field          = Modifier*FieldType*string*int*FieldOption list

    type ExtensionMember= int*int

    type Extension      = ExtensionMember list

    type EnumMember     =
        | EnumOption    of Option
        | EnumValue     of string*int

    type Enum           = string*EnumMember list

    type MessageMember  =
        | MemberField       of Field
        | MemberEnum        of Enum
        | MemberMessage     of Message
        | MemberExtend      of Extend
        | MemberExtension   of Extension
        | MemberGroup       of Group
        | MemberOption      of Option

    and Message         = string*MessageMember list

    and Group           = Modifier*int*Message

    and ExtendMember    =
        | ExtendField   of Field
        | ExtendGroup   of Group

    and Extend          = UserType*ExtendMember list

    type Package        = string list

    type Import         = string

    type ProtoMember    =
        | ProtoMessage  of Message
        | ProtoExtend   of Extend
        | ProtoEnum     of Enum
        | ProtoImport   of Import
        | ProtoPackage  of Package
        | ProtoOption   of Option

    type Proto          = ProtoMember list

    module Serializer = 

        open System.Globalization
        open System.Xml.Linq
(*
        let AsXDocument (p : Proto) = 
            let xdoc = XDocument()
            
            let children =
                [
                    for pm in p do
                        match pm with
                        | ProtoMessage  message -> yield serializeMessage   message 
                        | ProtoExtend   extend  -> yield serializeExtend    extend
                        | ProtoEnum     enum    -> yield serializeEnum      enum
                        | ProtoImport   import  -> yield serializeImport    import
                        | ProtoPackage  package -> yield serializePackage   package
                        | ProtoOption   option  -> yield serializeOption    option
                ]

            xdoc
            
*)