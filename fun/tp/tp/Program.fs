open System
open System.Data
open System.Data.SqlClient

open mrange.SqlSchema

[<EntryPoint>]
let main argv = 

    use connection = new SqlConnection("Data Source=localhost\SQLEXPRESS;Initial Catalog=TestDB;Integrated Security=True")
    connection.Open()
    let schema = Schema.New connection

    0
