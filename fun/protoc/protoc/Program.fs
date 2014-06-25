
open FParsec
open Primitives
open CharParsers

module Protobuf =

    type Value      = 
        | String    of string
        | Bool      of bool
        | Float     of float
        | Int       of int
        | Variable  of string

    type Modifier   = 
        | Required
        | Optional
        | Repeated

    type UserType   = bool*string list

    type FieldType  = 
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

    type Option     = string list*Value

    type FieldOption=
        | FieldOptionDefault    of Value
        | FieldOptionOption     of Option

    type Field      = Modifier*FieldType*string*int*FieldOption list

    type ExtensionMember= int*int

    type Extension      = ExtensionMember list

    type EnumMember     =
        | EnumOption    of Option
        | EnumField     of string*int

    type Enum           = string*EnumMember list

    type Group          = Modifier*int*Message

    type ExtendMember   =
        | ExtendField   of Field
        | ExtendGroup   of Group

    type Extend         = UserType*ExtendMember list

    type Message        = string*MessageMember list

    and MessageMember   =
        | MemberField       of Field
        | MemberEnum        of Enum
        | MemberMessage     of Message
        | MemberExtend      of Extend
        | MemberExtension   of Extension
        | MemberGroup       of Group
        | MemberOption      of Option

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
    
    module Internal = 

        let ws              = spaces
        let ws1             = spaces1
        let ch c            = skipChar c .>> ws
        let str s           = skipString s .>> ws
        let strRet s v      = stringReturn s v .>> ws

        let stringChoices cs= cs |> List.map (fun (s,v) -> attempt <| strRet s v )            

        let teq             = ch '='
        let tcomma          = ch ','
        let tsemicolon      = ch ';'
        let tlbracket       = ch '['
        let trbracket       = ch ']'

        let koption         = str "option" 
        let kdefault        = str "default"
        let kto             = str "to"
        let kextensions     = str "extensions"


        let identifierChar  : Parser<char, unit>    = letter <|> anyOf "_"

        let stringLiteral   : Parser<string, unit>  =
            let quote       = skipAnyOf "'\""
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
            let chars   = manyChars (char <|> charEscape)
            between quote quote chars .>> ws

        let stringValue     : Parser<Value, unit>   = 
            stringLiteral |>> Value.String

        let boolValue       : Parser<Value, unit>   = 
            let trueLit     = strRet "true"     true
            let falseLit    = strRet "false"    false
            trueLit <|> falseLit |>> Value.Bool

        let floatValue      : Parser<Value, unit>   = 
            pfloat .>> ws |>> Value.Float

        let intLiteral      : Parser<int, unit>     =
            let decInt      = pint32
            // TODO:
            // hexInt
            // octInt
            decInt .>> ws

        let intValue        : Parser<Value, unit>   = 
            intLiteral |>> Int

//        let variableValue   = many1Chars2 upper identChar |>> VariableValue
        
        let identifierLiteral   : Parser<string, unit>  = 
            many1Chars identifierChar

        let variableValue   : Parser<Value, unit>   = 
            identifierLiteral |>> Variable

        let value           : Parser<Value, unit>   = 
            choice
                [
                    boolValue
                    intValue
                    floatValue  // This won't properly as ints are also floats
                    variableValue
                    stringValue
                ]

        let modifier        : Parser<Modifier, unit>=  
            let choices = 
                [
                    "optional", Optional
                    "required", Required
                    "repeated", Repeated
                ] |> stringChoices
            choice choices
    
        let userTypeLiteral : Parser<string list, unit> = 
            sepBy1 stringLiteral (skipChar '.') .>> ws

        let userType        : Parser<UserType, unit>    = 
            pipe2 (opt <| skipChar '.') userTypeLiteral (fun fq ids -> fq.IsSome,ids) 
            

        let fieldType       : Parser<FieldType, unit>   = 
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

        let optionBody      : Parser<Option, unit>  = 
            pipe2 userTypeLiteral (teq >>. value) (fun id value -> id,value)

        let option          : Parser<Option, unit>  = 
            koption >>.optionBody .>> tsemicolon

        let fieldOption     : Parser<FieldOption, unit> =
            choice
                [
                    attempt optionBody |>> FieldOptionOption
                    kdefault >>. teq >>. value |>> FieldOptionDefault
                ]

        let fieldOptions    : Parser<FieldOption list, unit> =
            between tlbracket trbracket (sepBy1 fieldOption tcomma)

        let field           : Parser<Field, unit> =
            pipe5 
                (modifier .>> ws1) 
                (fieldType .>> ws1) 
                identifierLiteral
                (teq >>. intLiteral) 
                (opt fieldOptions)
                (fun m ft id i fos -> m, ft, id, i, (defaultArg fos []))

        let extensionMember : Parser<ExtensionMember, unit> =
            let part2 = kto >>. (intLiteral <|> strRet "max" System.Int32.MaxValue)
            pipe2 (intLiteral .>> ws) (attempt part2) (fun i1 i2 -> i1, i2) .>> ws

        let extension       : Parser<Extension, unit>   =
            kextensions
            >>. sepBy1 (extensionMember .>> ws) tcomma
            .>> tsemicolon 
            .>> ws

        let enumMember      : Parser<EnumMember, unit>  =
            choice 
                [
                    attempt option |>> EnumOption
                    pipe3 (identifierLiteral .>> ws) (teq >>.intLiteral .>> ws) (tsemicolon >>. ws) (fun id i _ -> EnumField (id, i))
                ]

//        let enum            : Parser<Enum, unit>        =
//            pipe3 
//                (str "enum" >>. ws >>. identifierLiteral .>> ws)
//                (between (ch '{' .>> ws) (ch '}' .>> ws) )


        let proto           : Parser<_,unit> = value

    
    let Parse (s : string) = 
        let result = run Internal.proto s
        result
    

let protoSpec1 = """
message Person {
  required int32 id = 1;
  required string name = 2;
  optional string email = 3;
}
"""
[<EntryPoint>]
let main argv = 
    0

