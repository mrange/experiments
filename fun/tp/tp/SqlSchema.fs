
namespace mrange

open System
open System.Data
open System.Data.SqlClient
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

[<AutoOpen>]
module Utilities =
    let inline coalesce a b = 
        match a with 
        |   Some a' -> a'
        |   _       -> b
    let inline ( <?> ) a b = coalesce a b

module SqlSchema =

    type SqlTypeInfo =
        {
            DbType              : SqlDbType  
            ClrType             : Type      
            DbElementSize       : int       
            RequiresDimension   : bool      
            DbDefaultValue      : string    
            GetterMethod        : MethodInfo
            DbTypeName          : string    
            CsTypeName          : string    
        }
        
        static member New dbType clrType dbElementSize requiresDimension dbDefaultValue getterMethod =
            {
                DbType              = dbType            
                ClrType             = clrType          
                DbElementSize       = dbElementSize    
                RequiresDimension   = requiresDimension
                DbDefaultValue      = dbDefaultValue   
                GetterMethod        = getterMethod     
                DbTypeName          = dbType.ToString().ToLowerInvariant()
                CsTypeName          = if clrType.IsArray then clrType.GetElementType().FullName + "[]"
                                        else clrType.GetElementType().FullName
            }

        static member Empty = SqlTypeInfo.New SqlDbType.Variant typeof<obj> -1 false "null" null


    type TypeDefinition =   
        {
            Schema          : string        
            Name            : string        
            SystemTypeId    : byte          
            UserTypeId      : int           
            MaxLength       : int16         
            Precision       : byte          
            Scale           : byte          
            Collation       : string        
            IsNullable      : bool          
            FullName        : string        
            TypeInfo        : SqlTypeInfo   
        }

        static member td (i : int) (dbType : SqlDbType) (dbElementSize : int) (requiresDimension : bool) (dbDefaultValue : string) (getter : Expr<(SqlDataReader -> 'T)>) (map : Map<byte, SqlTypeInfo>) = 
            match getter with 
            |   Lambda (_, Call (_, mi, _)) ->  map
                                                |> Map.add (byte i) (SqlTypeInfo.New dbType typeof<'T> dbElementSize requiresDimension dbDefaultValue mi)
            |   _                           ->  failwith "Wrongly configured getter method"

        static member std (i : int) (dbType : SqlDbType) (dbElementSize : int) (requiresDimension : bool) (dbDefaultValue : string) (map : Map<byte, SqlTypeInfo>) = 
            map
            |> Map.add (byte i) (SqlTypeInfo.New dbType null dbElementSize requiresDimension dbDefaultValue null)


        static member GetBytes (r : SqlDataReader) i =
            let bytes = r.GetSqlBytes(i)
            bytes.Buffer

        static member lookup : Map<byte, SqlTypeInfo> = 
            Map.empty
            //                      TypeId  SqlDbType                   Sz  Req?    DefaultValue    ValueGetter
            |> TypeDefinition.td    127     SqlDbType.BigInt            8   false   "0"             <@ (fun r -> r.GetInt64(0)              ) @>
            |> TypeDefinition.td    173     SqlDbType.Binary            1   true    "0x0"              
            |> TypeDefinition.td    104     SqlDbType.Bit               1   false   "0"             <@ (fun r -> r.GetBoolean(0)            ) @>
            |> TypeDefinition.td    175     SqlDbType.Char              1   true    "''"            <@ (fun r -> r.GetString(0)             ) @>
            |> TypeDefinition.td    61      SqlDbType.DateTime          8   false   "1900-01-01"    <@ (fun r -> r.GetDateTime(0)           ) @>
            |> TypeDefinition.td    106     SqlDbType.Decimal           17  false   "0"             <@ (fun r -> r.GetDecimal(0)            ) @>
            |> TypeDefinition.td    62      SqlDbType.Float             4   false   "0"             <@ (fun r -> r.GetDouble(0)             ) @>
            |> TypeDefinition.std   34      SqlDbType.Image             1   false   ""              
            |> TypeDefinition.td    56      SqlDbType.Int               4   false   "0"             <@ (fun r -> r.GetInt32(0)              ) @>
            |> TypeDefinition.td    60      SqlDbType.Money             8   false   "0"             <@ (fun r -> r.GetDecimal(0)            ) @>
            |> TypeDefinition.td    239     SqlDbType.NChar             2   true    "''"            <@ (fun r -> r.GetString(0)             ) @>
            |> TypeDefinition.td    99      SqlDbType.NText             2   false   "''"            <@ (fun r -> r.GetString(0)             ) @>
            |> TypeDefinition.td    231     SqlDbType.NVarChar          2   true    "''"            <@ (fun r -> r.GetString(0)             ) @>
            |> TypeDefinition.td    59      SqlDbType.Real              4   false   "0"             <@ (fun r -> r.GetFloat(0)              ) @>
            |> TypeDefinition.td    36      SqlDbType.UniqueIdentifier  1   false   ""              <@ (fun r -> r.GetGuid(0)               ) @>
            |> TypeDefinition.td    58      SqlDbType.SmallDateTime     4   false   "1900-01-01"    <@ (fun r -> r.GetDateTime(0)           ) @>
            |> TypeDefinition.td    52      SqlDbType.SmallInt          2   false   "0"             <@ (fun r -> r.GetInt16(0)              ) @>
            |> TypeDefinition.td    122     SqlDbType.SmallMoney        4   false   "0"             <@ (fun r -> r.GetDecimal(0)            ) @>
            |> TypeDefinition.td    35      SqlDbType.Text              1   false   "''"            <@ (fun r -> r.GetString(0)             ) @>
            |> TypeDefinition.td    189     SqlDbType.Timestamp         8   false   ""              <@ (fun r -> r.GetDateTime(0)           ) @>
            |> TypeDefinition.td    48      SqlDbType.TinyInt           1   false   "0"             <@ (fun r -> r.GetByte(0)               ) @>
            |> TypeDefinition.std   165     SqlDbType.VarBinary         1   true    ""              
            |> TypeDefinition.td    167     SqlDbType.VarChar           1   true    "''"            <@ (fun r -> r.GetString(0)             ) @>
            |> TypeDefinition.td    98      SqlDbType.Variant           -1  false   ""              <@ (fun r -> r.GetValue(0)              ) @>
            |> TypeDefinition.std   241     SqlDbType.Xml               -1  false   ""              
            |> TypeDefinition.td    40      SqlDbType.Date              8   false   "1900-01-01"    <@ (fun r -> r.GetDateTime(0)           ) @>
            |> TypeDefinition.td    41      SqlDbType.Time              5   false   ""              <@ (fun r -> r.GetDateTime(0)           ) @>
            |> TypeDefinition.td    42      SqlDbType.DateTime2         8   false   "1900-01-01"    <@ (fun r -> r.GetDateTime(0)           ) @>
            |> TypeDefinition.td    43      SqlDbType.DateTimeOffset    10  false   ""              <@ (fun r -> r.GetDateTimeOffset(0)     ) @>

        static member New schema name systemTypeId userTypeId maxLength precision scale collation isNullable =
            {
                Schema          = schema            
                Name            = name              
                SystemTypeId    = systemTypeId      
                UserTypeId      = userTypeId        
                MaxLength       = maxLength         
                Precision       = precision         
                Scale           = scale             
                Collation       = collation         
                IsNullable      = isNullable        
                FullName        = schema + "."+ name
                TypeInfo        = (TypeDefinition.lookup |> Map.tryFind systemTypeId) <?> SqlTypeInfo.Empty
            }

        static member Empty = TypeDefinition.New "" "" (byte 98) 98 (int16 -1) (byte 0) (byte 0) "" true

        type ObjectProperties =
            {
                Id          : int
                Schema      : string   
                Name        : string
                CreateDate  : DateTime
                ModifyDate  : DateTime   
                FullName    : string   
            }
            static member New id schema name createDate modifyDate = 
                {
                    Id          = id          
                    Schema      = schema      
                    Name        = name        
                    CreateDate  = createDate  
                    ModifyDate  = modifyDate  
                    FullName    = schema + "." + name
                }


        type ParameterSubObject =
            {   
                Name        : string          
                Type        : TypeDefinition  
                Id          : int             
                MaxLength   : int16
                Precision   : byte            
                Scale       : byte            
                IsOutput    : bool
            }
            static member New name ``type`` id maxLength precision scale isOutput =
                {
                    Name        = name        
                    Type        = ``type``
                    Id          = id          
                    MaxLength   = maxLength   
                    Precision   = precision   
                    Scale       = scale       
                    IsOutput    = isOutput  
                }

        type ColumnSubObject =
            {   
                Name        : string          
                Type        : TypeDefinition  
                Id          : int             
                MaxLength   : int16
                Precision   : byte            
                Scale       : byte            
                Collation   : string          
                IsNullable  : bool            
                IsIdentity  : bool            
                IsComputed  : bool            
            }
            static member New name ``type`` id maxLength precision scale collation isNullable isIdentity isComputed = 
                {
                    Name        = name        
                    Type        = ``type``
                    Id          = id          
                    MaxLength   = maxLength   
                    Precision   = precision   
                    Scale       = scale       
                    Collation   = collation   
                    IsNullable  = isNullable  
                    IsIdentity  = isIdentity  
                    IsComputed  = isComputed  
                }

        type SchemaObject =
            | Unknown                   of ObjectProperties
            | StoredProcedure           of ObjectProperties*ParameterSubObject array
            | Function                  of ObjectProperties*ParameterSubObject array*TypeDefinition
            | TableFunction             of ObjectProperties*ParameterSubObject array*ColumnSubObject array  
            | InlineTableFunction       of ObjectProperties*ParameterSubObject array*ColumnSubObject array  
            | Table                     of ObjectProperties*ColumnSubObject array  
            | View                      of ObjectProperties*ColumnSubObject array  

            member x.Common =   
                match x with
                | Unknown               p       -> p
                | StoredProcedure       (p,_)   -> p
                | Function              (p,_,_) -> p
                | TableFunction         (p,_,_) -> p
                | InlineTableFunction   (p,_,_) -> p
                | Table                 (p,_)   -> p
                | View                  (p,_)   -> p


        type Schema(typeDefinitions : Map<string, TypeDefinition>, schemaObjects : Map<string, SchemaObject>) = 

            member x.TypeDefinitions= typeDefinitions
            member x.SchemaObjects  = schemaObjects

            static member New (connection : SqlConnection) =
                use command = connection.CreateCommand()
                command.CommandType <- CommandType.Text;
                command.CommandText <- @"
SELECT
	s.name								[Schema]		,	-- 0
	t.name								Name			,	-- 1
	t.system_type_id					SystemTypeId	,	-- 2
	t.user_type_id						UserTypeId		,	-- 3
	t.max_length						[MaxLength]		,	-- 4
	t.[precision]						[Precision]		,	-- 5
	t.scale								Scale			,	-- 6
	ISNULL (t.collation_name	, '')	Collation		,	-- 7
	ISNULL (t.is_nullable		, 0)	IsNullable		 	-- 8
	FROM SYS.types t WITH(NOLOCK)
	INNER JOIN SYS.schemas s WITH(NOLOCK) ON t.schema_id = s.schema_id

SELECT 
	c.object_id							ObjectId	,	-- 0
	s.name								[TypeSchema],	-- 1
	t.name								[TypeName]	,	-- 2
	ISNULL (c.name		, '')			Name		,	-- 3
	c.column_id							Id          ,	-- 4
	c.max_length						[MaxLength]	,	-- 5
	c.[precision]						[Precision]	,	-- 6
	c.scale								Scale		,	-- 7
	ISNULL (c.collation_name	, '')	Collation	,	-- 8
	ISNULL (c.is_nullable		, 0)	IsNullable	,	-- 9
	c.is_identity						IsIdentity	,	-- 10
	c.is_computed						IsComputed		-- 11
	FROM SYS.objects o WITH(NOLOCK)
	INNER JOIN SYS.columns c WITH(NOLOCK) ON o.object_id = c.object_id
	INNER JOIN SYS.types t WITH(NOLOCK) ON c.user_type_id = t.user_type_id AND c.system_type_id = t.system_type_id
	INNER JOIN SYS.schemas s WITH(NOLOCK) ON t.schema_id = s.schema_id
	WHERE
		o.is_ms_shipped = 0

SELECT 
	p.object_id							ObjectId	,	-- 0
	s.name								[TypeSchema],	-- 1
	t.name								[TypeName]	,	-- 2
	ISNULL (p.name		, '')			Name		,	-- 3
	p.parameter_id						Id		    ,	-- 4
	p.max_length						[MaxLength]	,	-- 5
	p.[precision]						[Precision]	,	-- 6
	p.scale								Scale		,	-- 7
	p.is_output							IsOutput		-- 8
	FROM SYS.objects o WITH(NOLOCK)
	INNER JOIN SYS.parameters p WITH(NOLOCK) ON o.object_id = p.object_id
	INNER JOIN SYS.types t WITH(NOLOCK) ON p.user_type_id = t.user_type_id AND p.system_type_id = t.system_type_id
	INNER JOIN SYS.schemas s WITH(NOLOCK) ON t.schema_id = s.schema_id
	WHERE
		o.is_ms_shipped = 0

SELECT
	o.object_id							ObjectId		,	-- 0
	s.name								[Schema]		,	-- 1
	o.name								Name			,	-- 2
	o.[type]							[Type]			,	-- 3
	o.create_date						CreateDate		,	-- 4
	o.modify_date						ModifyDate			-- 5
	FROM SYS.schemas s WITH(NOLOCK)
	INNER JOIN SYS.objects o WITH(NOLOCK) ON o.schema_id = s.schema_id
	WHERE
		o.is_ms_shipped = 0
		AND
		o.type IN ('P', 'TF', 'IF', 'FN', 'U', 'V')
"
                use reader = command.ExecuteReader(CommandBehavior.SequentialAccess)
                let tds =   [|  while reader.Read() do yield TypeDefinition.New (reader.GetString(0)    )  
                                                                                (reader.GetString(1)    )
                                                                                (reader.GetByte(2)      )
                                                                                (reader.GetInt32(3)     )
                                                                                (reader.GetInt16(4)     )
                                                                                (reader.GetByte(5)      )
                                                                                (reader.GetByte(6)      )
                                                                                (reader.GetString(7)    )
                                                                                (reader.GetBoolean(8)   )
                            |]
                            |> Array.fold (fun s v -> Map.add v.FullName v s) Map.empty

                if not (reader.NextResult()) then failwith "Too few results returned by SQLServer"

                let folder (map : Map<int, 'T list>) ((oid : int),(v : 'T)) = 
                    let find = map.TryFind oid
                    match find with
                    | Some ls   -> map |> Map.remove oid |> Map.add oid (v::ls)
                    | _         -> map |> Map.add oid [v]

                let cols =  [|  while reader.Read() do  let objectId        = reader.GetInt32(0)
                                                        let typeFullname    = reader.GetString(1) + "." + reader.GetString(2)
                                                        let typeDefinition  = tds.TryFind typeFullname <?> TypeDefinition.Empty
                                                        
                                                        let col = ColumnSubObject.New   (reader.GetString(3)    ) 
                                                                                        (typeDefinition         ) 
                                                                                        (reader.GetInt32(4)     )  
                                                                                        (reader.GetInt16(5)     )  
                                                                                        (reader.GetByte(6)      )  
                                                                                        (reader.GetByte(7)      )  
                                                                                        (reader.GetString(8)    )  
                                                                                        (reader.GetBoolean(9)   )  
                                                                                        (reader.GetBoolean(10)  )  
                                                                                        (reader.GetBoolean(11)  ) 
                                                        yield objectId,col

                            |]
                            |> Array.fold folder Map.empty

                if not (reader.NextResult()) then failwith "Too few results returned by SQLServer"

                let pars =  [|  while reader.Read() do  let objectId        = reader.GetInt32(0)
                                                        let typeFullname    = reader.GetString(1) + "." + reader.GetString(2)
                                                        let typeDefinition  = tds.TryFind typeFullname <?> TypeDefinition.Empty
                                                        
                                                        let par = ParameterSubObject.New    (reader.GetString(3)    ) 
                                                                                            (typeDefinition         ) 
                                                                                            (reader.GetInt32(4)     )
                                                                                            (reader.GetInt16(5)     )
                                                                                            (reader.GetByte(6)      )
                                                                                            (reader.GetByte(7)      )
                                                                                            (reader.GetBoolean(8)   )
                                                        yield objectId,par
                            |]
                            |> Array.fold folder Map.empty

                if not (reader.NextResult()) then failwith "Too few results returned by SQLServer"

                let objs =  [|  while reader.Read() do  let objectId            = reader.GetInt32(0)
                                                        let schema              = reader.GetString(1)
                                                        let name                = reader.GetString(2)
                                                        let schemaObjectType    = reader.GetString(3)

                                                        let col = cols |> Map.tryFind objectId <?> [] |> List.sortBy (fun c -> c.Id) |> List.toArray
                                                        let par = pars |> Map.tryFind objectId <?> [] |> List.sortBy (fun p -> p.Id) |> List.toArray

                                                        let props= ObjectProperties.New objectId 
                                                                                        schema 
                                                                                        name
                                                                                        (reader.GetDateTime(4)) 
                                                                                        (reader.GetDateTime(5))

                                                        yield   match schemaObjectType with
                                                                | "P"   -> Unknown              props
                                                                | "U"   -> Table                (props,col)
                                                                | "TF"  -> TableFunction        (props,par,col)
                                                                | "IF"  -> InlineTableFunction  (props,par,col)
                                                                | "FN"  -> Function             (props, par, TypeDefinition.Empty)
                                                                | "V"   -> View                 (props, col)
                                                                | _     -> Unknown props
                
                            |] |> Array.fold (fun s v -> Map.add v.Common.FullName v s) Map.empty

                Schema(tds, Map.empty)


