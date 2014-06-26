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

open System

open protoc
open protobuf

open FParsec

let mutable errors = 0

let error (msg : string) = 
    errors <- errors + 1

    let cc = Console.ForegroundColor
    Console.ForegroundColor <- ConsoleColor.Red
    try
        printfn "Error detected: %s" msg
    finally
        Console.ForegroundColor <- cc



let runTestCases () =
    
    let mutable iter = 0

    for testDescription, testCase in TestCases.PositiveTestCases do
        iter <- iter + 1
        let result = protobuf.Parse testCase 
        
        match result with
        | Success (v, _, _) -> ()
        | Failure (m, _, _) -> error <| sprintf "Parser failed for test case #%d, %s" iter testDescription
        

[<EntryPoint>]
let main argv = 
    
    runTestCases ()

    if errors = 0 then
        0
    else
        101
