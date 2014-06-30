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

        let AsXDocument (p : Proto) = 
            
            // TODO: UserType doesn't serialize properly

            let culture = CultureInfo.InvariantCulture
            let inline format v = sprintf "%A" v
            let formatReference (ss : string list) = String.concat "." ss
            let formatUserType (ut : UserType) = 
                let fq, ref = ut
                sprintf "[%s]" <| formatReference ((if fq then "." else "")::ref)
            let formatFieldType (ft : FieldType) = 
                match ft with
                | UserType ut   -> formatUserType ut
                | _             -> format ft

            let inline xname n = XName.Get n

            let inline attr (n : string) (v : string) = 
                XAttribute (xname n, v)

            let ele (n : string) (attrs : seq<XAttribute>) (els : seq<XElement>) =
                let a : seq<obj>    = attrs |> Seq.cast
                let e : seq<obj>    = els |> Seq.cast
                let all             = Seq.concat [a;e] |> Seq.toArray
                XElement (xname n, all)

            let serializePackage (e : Package) : XElement = 
                ele "Package" [| attr "ref" (formatReference e)|] [||]

            let serializeExtensionMember (m : ExtensionMember) : XElement = 
                let f,t = m
                ele "ExtensionMember" 
                    [|
                        attr "from" (f.ToString culture)
                        attr "to"   (t.ToString culture)
                    |] [||]
                    
            let serializeExtension (e : Extension) : XElement = 
                ele "Extension" [||] (e |> List.map serializeExtensionMember)
                    
            let serializeImport (i : Import) : XElement = 
                ele "Import" [|attr "file" i|] [||]

            let serializeValue (v : Value) : XElement = 
                let tp, v = 
                    match v with
                    | StringValue   v -> "string"   , v
                    | BoolValue     v -> "bool"     , if v then "true" else "false"
                    | FloatValue    v -> "float"    , (v.ToString culture)
                    | IntValue      v -> "int"      , (v.ToString culture)
                    | VariableValue v -> "variable" , v

                ele "Value" [|attr "type" tp; attr "value" v|] [||]

            let serializeOption (o : Option) : XElement = 
                let ref, v = o
                ele "Option" [|attr "ref" (formatReference ref)|] [|serializeValue v|]

            let serializeEnum (e : Enum) : XElement = 
                let name, members = e
                ele "Enum" [||] 
                    [|
                        for m in members do
                        match m with
                        | EnumValue (n,v)   -> yield ele "EnumValue" [| attr "name" n; attr "value" (v.ToString culture) |] [||]
                        | EnumOption e      -> yield serializeOption e
                    |]

            let serializeField (f : Field) : XElement = 
                let m, ft, name, idx, options = f
                ele "Field" 
                    [|
                        attr "modifier"     (format m)
                        attr "fieldType"    (formatFieldType ft)
                        attr "name"         name
                        attr "id"           (idx.ToString culture)  
                    |]
                    [||]

            let rec serializeGroup (g : Group) : XElement = 
                let m, idx, message = g
                ele "Group" 
                    [| 
                        attr "modifier" (format m) 
                        attr "id"       (idx.ToString culture)  
                    |]
                    [|
                        serializeMessage message
                    |]

            and serializeExtend (e : Extend) : XElement = 
                let ref, members = e
                ele "Extend" 
                    [| attr "userType" (formatUserType ref) |]
                    [|
                        for m in members do
                        match m with
                        | ExtendField   f -> yield serializeField f
                        | ExtendGroup   g -> yield serializeGroup g
                    |]

            and serializeMessage (m : Message) : XElement = 
                let name, members = m
                ele "Message"
                    [|
                        attr "name" name  
                    |]
                    [|
                        for m in members do
                        match m with
                        | MemberField       f       -> yield serializeField     f
                        | MemberEnum        e       -> yield serializeEnum      e
                        | MemberMessage     m       -> yield serializeMessage   m
                        | MemberExtend      e       -> yield serializeExtend    e
                        | MemberExtension   e       -> yield serializeExtension e
                        | MemberGroup       g       -> yield serializeGroup     g
                        | MemberOption      o       -> yield serializeOption    o
                    |]
                


            XDocument (
                [|
                    for pm in p do
                        match pm with
                        | ProtoMessage  m   -> yield serializeMessage   m 
                        | ProtoExtend   e   -> yield serializeExtend    e
                        | ProtoEnum     e   -> yield serializeEnum      e
                        | ProtoImport   i   -> yield serializeImport    i
                        | ProtoPackage  p   -> yield serializePackage   p
                        | ProtoOption   o -> yield serializeOption      o
                |])
            
