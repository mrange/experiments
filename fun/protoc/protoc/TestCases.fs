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

namespace Protobuf.Tests

module TestCases =
    let PositiveTestCases =
        [
            "Empty message", """
message Person {
}
"""
            // Simple case involving a field
            "Message with a field", """
message Person {
  required int32 id = 1;
}
"""
            "Message with three fields", """
message Person {
  required int32 id = 1;
  required string name = 2;
  optional string email = 3;
}
"""
            "Message with three fields and default values", """message Person {
  required int32 id     = 1 [default = -1]          ;
  required string name  = 2 [default = "a"]   ;
  optional string email = 3;
}
"""
            "Enumeration", """
enum PhoneType {
    MOBILE = 0;
    HOME = 1;
    WORK = 2;
}
"""
            "Message referencing enum value", """
enum PhoneType {
    MOBILE = 0;
    HOME = 1;
    WORK = 2;
}

message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
  required string number = 4;
  optional PhoneType type = 5;
}
"""
            "Message with nested enum and message", """
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;

  enum PhoneType {
    MOBILE = 0;
    HOME = 1;
    WORK = 2;
  }

  message PhoneNumber {
    required string number = 1;
    optional PhoneType type = 2 [default = HOME];
  }

  repeated PhoneNumber phone = 4;
}
"""
            "Message with default enum value", """
message SearchRequest {
  required string query = 1;
  optional int32 page_number = 2;
  optional int32 result_per_page = 3 [default = 10];
  enum Corpus {
    UNIVERSAL = 0;
    WEB = 1;
    IMAGES = 2;
    LOCAL = 3;
    NEWS = 4;
    PRODUCTS = 5;
    VIDEO = 6;
  }
  optional Corpus corpus = 4 [default = UNIVERSAL];
}
"""
            "Message with UserType", """
message SearchResponse {
  repeated Result result = 1;
}

message Result {
  required string url = 1;
  optional string title = 2;
  repeated string snippets = 3;
}
"""
        ]


    let SerializeTestCase = """
message SearchRequest {
  required string query = 1;
  optional int32 page_number = 2;
  optional int32 result_per_page = 3 [default = 10];
  enum Corpus {
    UNIVERSAL = 0;
    WEB = 1;
    IMAGES = 2;
    LOCAL = 3;
    NEWS = 4;
    PRODUCTS = 5;
    VIDEO = 6;
  }
  optional Corpus corpus = 4 [default = UNIVERSAL];
}
"""