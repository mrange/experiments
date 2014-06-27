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

open Protobuf.Tests
open Protobuf.TypeProvider.Parser

let mutable errors = 0

let print (color : ConsoleColor) (msg : string) = 
    let cc = Console.ForegroundColor
    Console.ForegroundColor <- color
    try
        printfn "%s" msg
    finally
        Console.ForegroundColor <- cc

let info (msg : string) = 
    print ConsoleColor.Gray msg

let result (msg : string) = 
    print ConsoleColor.White msg

let success (msg : string) = 
    print ConsoleColor.Green msg

let error (msg : string) =
    errors <- errors + 1
    print ConsoleColor.Red msg


type RunTestCase =
    | RunAll
    | PrintResult
    | RunOne        of int

let runTestCases (rtc : RunTestCase) =

    let mutable iter    = 0

    let runOne =
        match rtc with
        | RunAll        -> None
        | PrintResult   -> None
        | RunOne i      -> Some i

    for testDescription, testCase in TestCases.PositiveTestCases do
        iter <- iter + 1

        let runTest = defaultArg runOne -1

        if runTest = -1 || runTest = iter then
            let r = ParseProtobuf testCase

            match r, rtc with
            | ParseSuccess _    , RunAll -> ()
            | ParseSuccess v    , _      -> result<| sprintf 
                                                        "Parser successful for test case #%d, %s\nInput:\n%s\nResult:\n%A"
                                                        iter 
                                                        testDescription 
                                                        testCase
                                                        v
            | ParseFailure _    , RunAll -> error <| sprintf "Parser failed for test case #%d, %s" iter testDescription
            | ParseFailure m    , _      -> error <| sprintf 
                                                        "Parser failed for test case #%d, %s\nInput:\n%s\nError:\n%s" 
                                                        iter 
                                                        testDescription 
                                                        testCase
                                                        m


[<EntryPoint>]
let main argv =

    info "Running test cases"
    
//    runTestCases PrintResult
//    runTestCases <| RunOne 5
    runTestCases RunAll

    if errors = 0 then
        success "All tests passed"
        0
    else
        error <| sprintf "%d errors were detected" errors
        101
