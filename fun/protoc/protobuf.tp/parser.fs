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

open FParsec
open Primitives
open CharParsers

open Protobuf
open ProtoSpecification

module ProtobufParser =

    module internal Internal =


        let ws              = spaces
        let ws1             = spaces1
        let ch c            = skipChar c .>> ws
        let str s           = skipString s .>> ws1
        let strRet s v      = stringReturn s v .>> ws
        let strRet1 s v     = stringReturn s v .>> ws1

        let stringChoices cs= cs |> List.map (fun (s,v) -> attempt <| strRet1 s v )

        let teq             = ch '='
        let tcomma          = ch ','
        let tsemicolon      = ch ';'
        let tlbracket       = ch '['
        let trbracket       = ch ']'
        let tlcbracket      = ch '{'
        let trcbracket      = ch '}'

        let koption         = str "option"
        let kdefault        = str "default"
        let kto             = str "to"
        let kextensions     = str "extensions"
        let kmessage        = str "message"
        let kgroup          = str "group"
        let kextend         = str "extend"
        let kenum           = str "enum"
        let kpackage        = str "package"
        let kimport         = str "import"

        let body  m         = between tlcbracket trcbracket (many m)


        let debug (info : string) (p : Parser<'T, 'S>)  : Parser<'T, 'S> = fun stream ->

            let r = p stream

            if r.Status = ReplyStatus.Ok then
                r            
            else
                r

        let identifierChar      : Parser<char, unit>                =
            letter <|> anyOf "_"

        let stringLiteral       : Parser<string, unit>              =
            let quote       = anyOf "\"'"
            let char        = noneOf "\0\n'\"\\"
            let charEscape  = anyOf """abfnrtv\?"'"""
                              |>> function
                                  | 'a' -> '\a'
                                  | 'b' -> '\b'
                                  | 'f' -> '\f'
                                  | 'n' -> '\n'
                                  | 'r' -> '\r'
                                  | 't' -> '\t'
                                  | 'v' -> '\v'
                                  | c   -> c
            let escaped     = skipChar '\\' >>.
                              choice
                                [
                                    // TODO:
                                    // OctEscape
                                    // HexEscape
                                    charEscape
                                ]
            let chars       = manyChars (char <|> escaped)
            between quote quote chars .>> ws

        let stringValue         : Parser<Value, unit>               =
            stringLiteral |>> StringValue

        let boolValue           : Parser<Value, unit>               =
            let trueLit     = strRet "true"     true
            let falseLit    = strRet "false"    false
            trueLit <|> falseLit |>> BoolValue

        let floatValue          : Parser<Value, unit>               =
            pfloat .>> ws |>> FloatValue

        let intLiteral          : Parser<int, unit>                 =
            let decInt      = pint32
            // TODO:
            // hexInt
            // octInt
            decInt .>> ws

        let intValue            : Parser<Value, unit>               =
            intLiteral |>> IntValue

        let camelIdentifierLiteral  : Parser<string, unit>          =
            many1Chars2 upper identifierChar .>> ws

        let identifierLiteral   : Parser<string, unit>              =
            many1Chars identifierChar .>> ws

        let variableValue       : Parser<Value, unit>               =
            identifierLiteral |>> VariableValue

        let value               : Parser<Value, unit>               =
            choice
                [
                    boolValue
                    intValue
                    floatValue  // This won't properly as ints are also floats
                    variableValue
                    stringValue
                ]

        let modifier            : Parser<Modifier, unit>            =
            let choices =
                [
                    "optional", Optional
                    "required", Required
                    "repeated", Repeated
                ] |> stringChoices
            choice choices

        let reference           : Parser<string list, unit>         =
            sepBy1 identifierLiteral (skipChar '.') .>> ws

        let userType            : Parser<UserType, unit>            =
            pipe2 (opt <| skipChar '.') reference (fun fq ids -> fq.IsSome,ids)


        let fieldType           : Parser<FieldType, unit>           =
            let choices =
                [
                    "double"    , Double
                    "float"     , Float
                    "int32"     , Int32
                    "int64"     , Int64
                    "uint32"    , UInt32
                    "uint64"    , UInt64
                    "sint32"    , SInt32
                    "sint64"    , SInt64
                    "fixed32"   , Fixed32
                    "fixed64"   , Fixed64
                    "sfixed32"  , SFixed32
                    "sfixed64"  , SFixed64
                    "bool"      , Bool
                    "string"    , String
                    "bytes"     , Bytes
                ] |> stringChoices
            let extraChoices =
                [
                    userType |>> UserType
                ]
            choice (choices @ extraChoices)

        let optionBody          : Parser<Option, unit>              =
            pipe2 reference (teq >>. value) (fun id value -> id,value)

        let option              : Parser<Option, unit>              =
            koption >>.optionBody .>> tsemicolon

        let fieldOption         : Parser<FieldOption, unit>         =
            choice
                [
                    attempt (kdefault >>. teq >>. value) |>> FieldDefaultValue
                    attempt optionBody |>> FieldValue
                ]

        let fieldOptions        : Parser<FieldOption list, unit>    =
            between tlbracket trbracket (sepBy1 fieldOption tcomma)

        let field               : Parser<Field, unit>               =
            pipe5
                modifier
                fieldType
                identifierLiteral
                (teq >>. intLiteral)
                (opt fieldOptions .>> tsemicolon)
                (fun m ft id i fos -> m, ft, id, i, (defaultArg fos []))

        let extensionMember : Parser<ExtensionMember, unit> =
            let part2 = kto >>. (intLiteral <|> strRet "max" System.Int32.MaxValue)
            pipe2 intLiteral (attempt part2) (fun i1 i2 -> i1, i2)

        let extension           : Parser<Extension, unit>           =
            kextensions
            >>. sepBy1 extensionMember tcomma
            .>> tsemicolon

        let enumMember          : Parser<EnumMember, unit>          =
            choice
                [
                    attempt option |>> EnumOption
                    attempt (pipe3 identifierLiteral (teq >>.intLiteral) tsemicolon (fun id i _ -> EnumValue (id, i)))
                ]

        let enumBody            : Parser<EnumMember list, unit>     =
            body enumMember

        let enum                : Parser<Enum, unit>                =
            pipe2
                (kenum >>. identifierLiteral)
                enumBody                            // TODO: support empty ';'
                (fun id members -> id, members)


        let messageMember, messageMemberRef                         =
            createParserForwardedToRef<MessageMember, unit> ()

        let messageBody         : Parser<MessageMember list, unit>  =
            body messageMember

        let message             : Parser<Message, unit>             =
            pipe2
                (kmessage >>. identifierLiteral)
                messageBody
                (fun id body -> id, body)

        let group               : Parser<Group, unit>               =
            pipe4
                modifier
                (kgroup >>. camelIdentifierLiteral)
                (teq >>. intLiteral)
                messageBody
                (fun m id i body -> m, i, (id, body))


        let extendMember    : Parser<ExtendMember, unit >           =
            choice
                [
                    attempt field |>> ExtendField
                    attempt group |>> ExtendGroup
                ]
        let extendBody      : Parser<ExtendMember list, unit>       =
            body extendMember

        let extend          : Parser<Extend, unit>                  =
            pipe2
                (kextend >>. userType)
                extendBody
                (fun ut b -> ut, b)

        messageMemberRef :=
            choice
                [
                    attempt field       |>> MemberField
                    attempt enum        |>> MemberEnum
                    attempt message     |>> MemberMessage
                    attempt extend      |>> MemberExtend
                    attempt extension   |>> MemberExtension
                    attempt group       |>> MemberGroup
                    attempt option      |>> MemberOption
                    // TODO: support empty ';'
                ]


        let package         : Parser<Package, unit>                 =
            kpackage >>. reference .>> tsemicolon

        let import          : Parser<Import, unit>                  =
            kimport >>. stringLiteral .>> tsemicolon

        let protomember     : Parser<ProtoMember, unit>             =
            choice
                [
                    attempt message |>> ProtoMessage
                    attempt extend  |>> ProtoExtend
                    attempt enum    |>> ProtoEnum
                    attempt import  |>> ProtoImport
                    attempt package |>> ProtoPackage
                    attempt option  |>> ProtoOption
                    // TODO: support empty ';'
                ]

        let proto           : Parser<Proto,unit>                    =
            ws >>. many1 protomember .>> eof


    type ParseResult =
        | ParseSuccess  of Proto
        | ParseFailure   of string

    let ParseProtobuf (s : string) =
        let result = run Internal.proto s
        match result with
        | Success (v, _, _) -> ParseSuccess v
        | Failure (m, _, _) -> ParseFailure m

