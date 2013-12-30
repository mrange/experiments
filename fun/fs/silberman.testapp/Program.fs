open silberman

open Fundamental

open Logical
open Logical.Events
open Logical.Properties

[<EntryPoint>]
let main argv = 
    let body = 
        Stack 
            [
                Orientation.Value FromTop
            ]
            [
                Label 
                    [ 
                        Margin  .Value <| Thickness.Uniform 4.F
                        Text    .Value "Hi there!" 
                    ]
                TextButton "Click me!"
                    [ 
                    ]
                    >>+ Clicked.Handler (fun e v -> true)
                Label 
                    [ 
                        Margin  .Value <| Thickness.Uniform 4.F
                        Text    .Value "Hi there!" 
                    ]

            ]

    Application.Show "Test app" 1600 1200 body

    0
