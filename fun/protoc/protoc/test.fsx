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





