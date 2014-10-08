
module glimmer =

    module model =
        
        type DataType =
        |   Decimal
        |   String

        type ColumDefinition(id : string, cdt : DataType) =

            member x.Id         = id
            member x.DataType   = cdt

        type TableDefinition(id : string, columns : ColumDefinition[]) =

            member x.Id         = id
            member x.Columns    = columns

    module data =
        
        open model

        type Row(table : Table) = 

            member x.Table = table

        and Table(tableDefinition : TableDefinition) = 

            member x.TableDefinition    = tableDefinition


    
    module extension =

        type Test() = 
            
            member x.Tester() = ()

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0
