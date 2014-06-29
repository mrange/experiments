#I """bin\Debug\"""
#r "protobuf.tp"

open Protobuf.TypeProvider

[<Literal>]
let protoSpec =  """
message Person {
  required int32 id     = 1 [default = -1]          ;
  required string name  = 2 [default = "No name"]   ;
  optional string email = 3                         ;
}
"""

type proto  = ProtobufTyped<protoSpec>

let p       = proto.Person()
//p.id        <- 314





