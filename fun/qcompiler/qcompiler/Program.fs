
module QCompiler = 
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns

    open OptimizedClosures

    open System
    open System.Linq.Expressions
    open System.Reflection

    module Details = 
        
        let takeUpto (n : int) (xs : 'T list) : ('T list)*('T List) =
            let mutable result  = []
            let mutable input   = xs
            while result.Length < n && input.Length > 0 do
                result  <- input.Head::result
                input   <- input.Tail
            result |> List.rev,input
            

        let getStaticMethodInfo (a : Expr<'T>) : MethodInfo     = 
            match a with
            | Call (_, mi, _)   -> mi
            | _                 -> failwith "getStaticMethodInfo expects a Call expression"
            
        let getConstructorInfo (a : Expr<'T>) : ConstructorInfo = 
            match a with
            | NewObject (ci, _) -> ci
            | _                 -> failwith "getStaticMethodInfo expects a NewObject expression"

        let bflags          = BindingFlags.Public ||| BindingFlags.Static 

        let typeOfFFunc     = typedefof<FSharpFunc<_, _>>
        
        type FSharpFuncAdapter<'U>(func : Func<'U>) =
            inherit FSharpFunc<unit, 'U>() 
                override x.Invoke _                 = func.Invoke ()

        type FSharpFuncAdapter<'T1, 'U>(func : Func<'T1, 'U>) =
            inherit FSharpFunc<'T1, 'U>() 
                override x.Invoke p1                = func.Invoke p1

        type FSharpFuncAdapter<'T1, 'T2, 'U>(func : Func<'T1, 'T2, 'U>) =
            inherit FSharpFunc<'T1, 'T2, 'U>() 
                override x.Invoke p1                = fun p2 -> func.Invoke(p1,p2)
                override x.Invoke (p1,p2)           = func.Invoke(p1,p2)

        type FSharpFuncAdapter<'T1, 'T2, 'T3, 'U>(func : Func<'T1, 'T2, 'T3, 'U>) =
            inherit FSharpFunc<'T1, 'T2, 'T3, 'U>() 
                override x.Invoke p1                = fun p2 p3 -> func.Invoke(p1,p2,p3)
                override x.Invoke (p1,p2,p3)        = func.Invoke(p1,p2,p3)

        type FSharpFuncAdapter<'T1, 'T2, 'T3, 'T4, 'U>(func : Func<'T1, 'T2, 'T3, 'T4, 'U>) =
            inherit FSharpFunc<'T1, 'T2, 'T3, 'T4, 'U>() 
                override x.Invoke p1                = fun p2 p3 p4 -> func.Invoke(p1,p2,p3, p4)
                override x.Invoke (p1,p2,p3,p4)     = func.Invoke(p1,p2,p3,p4)

        type FSharpFuncAdapter<'T1, 'T2, 'T3, 'T4, 'T5, 'U>(func : Func<'T1, 'T2, 'T3, 'T4, 'T5, 'U>) =
            inherit FSharpFunc<'T1, 'T2, 'T3, 'T4, 'T5, 'U>() 
                override x.Invoke p1                = fun p2 p3 p4 p5 -> func.Invoke(p1,p2,p3, p4, p5)
                override x.Invoke (p1,p2,p3,p4,p5)  = func.Invoke(p1,p2,p3,p4,p5)

        let typeOfAdapters  = [|
                                typedefof<FSharpFuncAdapter<_>>
                                typedefof<FSharpFuncAdapter<_, _>>
                                typedefof<FSharpFuncAdapter<_, _, _>>
                                typedefof<FSharpFuncAdapter<_, _, _, _>>
                                typedefof<FSharpFuncAdapter<_, _, _, _, _>>
                                typedefof<FSharpFuncAdapter<_, _, _, _, _, _>>
                              |]

        let typeOfTuples    = [|
                                typeof<Tuple>
                                typedefof<Tuple<_>>
                                typedefof<Tuple<_, _>>
                                typedefof<Tuple<_, _, _>>
                                typedefof<Tuple<_, _, _, _>>
                                typedefof<Tuple<_, _, _, _, _>>
                                typedefof<Tuple<_, _, _, _, _, _>>
                                typedefof<Tuple<_, _, _, _, _, _, _>>
                                typedefof<Tuple<_, _, _, _, _, _, _, _>>
                              |]

        // Do generic caches

        [<CustomEquality>]
        [<CustomComparison>]
        type VariableDefinition  = 
            {
                Type    : Type
                Name    : string
            }

            static member New t n = {Type = t; Name = n}                   

            override x.Equals yobj = 
                match yobj with
                | :? VariableDefinition as y -> 
                    x.Type = y.Type && x.Name = y.Name
                | _ -> false

            override x.GetHashCode () = x.Type.GetHashCode () + x.Name.GetHashCode ()

            override x.ToString () = sprintf "{VD, Type:'%s', Name:'%s'}" x.Type.FullName x.Name

            interface IComparable with
                member x.CompareTo yobj = 
                    match yobj with
                    | :? VariableDefinition as y -> 
                        let comp = x.Type.FullName.CompareTo (y.Type.FullName)
                        if comp = 0 then x.Name.CompareTo y.Name
                        else comp
                    | _ -> -1

        type VariableExpressions = Map<VariableDefinition, ParameterExpression>


        let inlineApplication (expr : Expr) = 
            let rec opti (expr : Expr) = 
                match expr with 
                | Application (Lambda (v, l_e), a_e)    ->  [v,a_e],l_e
                | Application (i_e, a_e)                ->  let o_l,o_e = opti  i_e
                                                            match o_l with
                                                            | []    ->  [],Expr.Application (i_e, a_e)
                                                            | _     ->  let io_l,io_e = opti <| Expr.Application (o_e, a_e)
                                                                        o_l@io_l,io_e
                | _ -> [],expr

            let o_l,o_e = opti expr
            List.foldBack (fun (v,a) s -> Expr.Let (v, a, s)) o_l o_e


        let rec foldExpr_Opt    (folder: Expr -> 'T -> 'T) (expr : Expr option) (state : 'T): 'T = 
            match expr with
            | Some e    -> foldExpr folder e state
            | _         -> state
        and foldExpr_Many       (folder: Expr -> 'T -> 'T) (expr : Expr list) (state : 'T)  : 'T = 
            expr |> List.fold (fun s e -> foldExpr folder e s) state
        and foldExpr            (folder: Expr -> 'T -> 'T) (expr : Expr) (state : 'T)       : 'T = 
            match expr with
            | DefaultValue          _  
            | Value                 _                   
            | Var                   _               -> state |> folder expr

            | AddressOf             f               
            | Coerce                (f,_)           
            | Lambda                (_,f)   
            | NewDelegate           (_,_,f)
            | Quote                 f
            | TupleGet              (f,_)
            | TypeTest              (f,_)
            | UnionCaseTest         (f,_)
            | VarSet                (_,f)           -> state |> foldExpr folder f |> folder expr

            | AddressSet            (f, s)          
            | Application           (f, s)  
            | Let                   (_, f, s)
            | Sequential            (f, s)          
            | TryFinally            (f, s)
            | WhileLoop             (f, s)          -> state |> foldExpr folder f |> foldExpr folder s |> folder expr

            | IfThenElse            (f, s, t)       
            | ForIntegerRangeLoop   (_, f, s, t)    
            | TryWith               (f, _, s, _, t) -> state |> foldExpr folder f |> foldExpr folder s |> foldExpr folder t |> folder expr

            | FieldGet              (f, _)          -> state |> foldExpr_Opt folder f |> folder expr
            | NewArray              (_, f)
            | NewObject             (_, f)
            | NewRecord             (_, f)
            | NewTuple              f
            | NewUnionCase          (_, f)          -> state |> foldExpr_Many folder f |> folder expr

            | PropertyGet           (f, _, s)
            | Call                  (f, _, s)       -> state |> foldExpr_Opt folder f |> foldExpr_Many folder s |> folder expr

            | FieldSet              (f, _, s)       -> state |> foldExpr_Opt folder f |> foldExpr folder s |> folder expr

            | LetRecursive          (f, s)          -> let m = f |> List.map (fun (_,r) -> r)
                                                       state |> foldExpr_Many folder m |> foldExpr folder s |> folder expr

            | PropertySet           (f, _, s, t)    -> state |> foldExpr_Opt folder f |> foldExpr_Many folder s |> foldExpr folder t |> folder expr
            | _                     -> failwith "Unhandled case"

        let rec createTupleExpression (es : Expression list) = 
            match es with
            | []                            -> null :> Expression
            | x1::[]                        -> x1
            | _                             -> 
                let xs,rest = es |> takeUpto 7
                let ps = 
                    if rest.IsEmpty then
                        xs
                    else
                        let rest    = createTupleExpression rest
                        xs@[rest]
                let ts      = ps |> List.map (fun x -> x.Type) |> List.toArray
                let gt      = typeOfTuples.[ps.Length]
                let t       = gt.MakeGenericType ts
                let ci      = t.GetConstructor ts
                let new_q   = Expression.New (ci, ps)
                new_q :> Expression

        let newVariable (vars : VariableExpressions) (v : Var) =
            let vd      = VariableDefinition.New v.Type v.Name
            let v_q     = Expression.Variable (v.Type, v.Name)
            let vars    = vars |> Map.add vd v_q
            vars,v_q

        let rec toLinqExpression_Opt (vars : VariableExpressions) (expr : Expr option) : Expression =
            match expr with
            | Some e    -> toLinqExpression vars e
            | _         -> null :> Expression
        and toLinqExpression_Many (vars : VariableExpressions) (expr : Expr list) : Expression list =
            expr
            |> List.map (fun e -> toLinqExpression vars e)
        and toLinqExpression_ManyMany (vars : VariableExpressions) (expr : Expr list list) : Expression list list=
            expr
            |> List.map (fun e -> toLinqExpression_Many vars e)
        and toLinqExpression (vars : VariableExpressions) (expr : Expr) : Expression = 
            match expr with
            | AndAlso               (f_e,s_e)           ->
                let f_e     = toLinqExpression vars f_e
                let s_e     = toLinqExpression vars s_e
                let aa_q    = Expression.AndAlso (f_e, s_e)
                aa_q :> Expression
            | Application           (f_e,_)             ->
                // TODO:

                let rec buildExpression (f : Expression) (ess : Expression list list) = 
                    let rec returnType (t : Type)   = 
                        let args = t.GetGenericArguments()
                        if args.Length = 0 then
                            t
                        else
                            returnType args.[args.Length - 1]
                    match ess with
                    | []                        -> f
                    | x1::[]                    -> 
                        let p1  = createTupleExpression x1
                        let mi  = f.Type.GetMethod ("Invoke", [|p1.Type|])
                        let a_q = Expression.Call (f, mi, [|p1|])
                        a_q :> Expression
                    | _                         ->
                        let xs,rest = ess |> takeUpto 5
                        let txs     = xs |> List.map (fun x -> createTupleExpression x)
                        let ps      = f::txs
                        let fs,rs   = txs |> List.map (fun x -> x.Type) |> takeUpto 2
                        let fft     = typeOfFFunc.MakeGenericType(fs |> List.toArray);
                        let ts      = rs@[returnType f.Type]
                        let gmis    = fft.GetMethods(bflags) 
                        let gmi     = gmis 
                                        |> Array.find (fun mi -> mi.IsStatic && mi.Name = "InvokeFast" && mi.GetParameters().Length = (xs.Length + 1))
                        let mi      = gmi.MakeGenericMethod (ts |> List.toArray)
                        let a_q = Expression.Call(null, mi, ps)
                        if rest.IsEmpty then
                            a_q :> Expression
                        else
                            buildExpression a_q rest

                let i_e = inlineApplication expr

                match i_e with
                | Applications (f_e, arg_es)    -> 
                    let f_q     = toLinqExpression vars f_e
                    let arg_qs  = toLinqExpression_ManyMany vars arg_es
                    let a_q     = buildExpression f_q arg_qs
                    a_q
                | _                             -> toLinqExpression vars i_e
                
            | Call                  (t_e, mi, a_es)     ->
                let t_q     = toLinqExpression_Opt vars t_e
                let a_qs    = toLinqExpression_Many vars a_es
                let c_q     = Expression.Call (t_q, mi, a_qs)
                c_q :> Expression
            | Coerce                (f_e, t)            ->
                let f_q     = toLinqExpression vars f_e
                let c_q     = Expression.Convert (f_q, t)
                c_q :> Expression
            | DefaultValue          t                   ->
                let c_q     = Expression.Default t
                c_q :> Expression
            | FieldGet              (t_e, fi)           ->
                let t_q     = toLinqExpression_Opt vars t_e
                let fg_q    = Expression.Field (t_q, fi)
                fg_q :> Expression
            | FieldSet              (t_e, fi, a_e)      ->
                let t_q     = toLinqExpression_Opt vars t_e
                let a_q     = toLinqExpression vars a_e
                let f_q     = Expression.Field (t_q, fi)
                let a_q     = Expression.Assign (f_q, a_q);
                a_q :> Expression
            | ForIntegerRangeLoop   (v, b_e, e_e, l_e)  ->
                let start       = Expression.Label ()   
                let test        = Expression.Label ()
                let startLabel_q= Expression.Label start    
                let testLabel_q = Expression.Label test     
                let gotoStart_q = Expression.Goto start     
                let gotoTest_q  = Expression.Goto test      

                let b_q         = toLinqExpression vars b_e
                let e_q         = toLinqExpression vars e_e
                let vars,v_q    = newVariable vars v
                let l_q         = toLinqExpression vars l_e
                let a_q         = Expression.Assign (v_q, b_q)
                let inc_q       = Expression.PreIncrementAssign (v_q);
                let t_q         = Expression.LessThanOrEqual (v_q, e_q)
                let if_q        = Expression.IfThen (t_q, gotoStart_q)

                let b_q         = Expression.Block ([|v_q|], a_q, gotoTest_q, startLabel_q, l_q, testLabel_q, inc_q, if_q)
                b_q :> Expression
            | IfThenElse            (if_e, t_e, f_e)    ->
                let if_q    = toLinqExpression vars if_e
                let t_q     = toLinqExpression vars t_e
                let f_q     = toLinqExpression vars f_e
                let t_q     = Expression.Condition (if_q, t_q, f_q, t_q.Type)
                t_q :> Expression
            | Lambdas               (vs, e_e)           ->
                null :> Expression

            | Let                   (v, let_e, in_e)    ->
                // TODO: Coalesce lets in sequence
                let let_q   = toLinqExpression vars let_e
                let vars,v_q= newVariable vars v
                let a_q     = Expression.Assign (v_q, let_q)
                let in_q    = toLinqExpression vars in_e
                let b_q     = Expression.Block ([v_q], a_q, in_q)
                b_q :> Expression
            | NewArray              (t, a_es)           ->
                let a_qs    = toLinqExpression_Many vars a_es
                let n_q     = Expression.NewArrayInit (t, a_qs)
                n_q :> Expression
            | NewObject             (ci, a_es)          ->
                let a_qs    = toLinqExpression_Many vars a_es
                let n_q     = Expression.New (ci, a_qs)
                n_q :> Expression
            | NewTuple              (a_es)              ->
                let a_qs    = createTupleExpression <| toLinqExpression_Many vars a_es
                a_qs
            | PropertyGet           (t_e, pi, i_es)     ->
                let t_q     = toLinqExpression_Opt vars t_e
                let i_qs    = toLinqExpression_Many vars i_es
                let pg_q    = Expression.Property (t_q, pi, i_qs)
                pg_q :> Expression
            | PropertySet           (t_e, pi, i_es, a_e)    ->
                let t_q     = toLinqExpression_Opt vars t_e
                let i_qs    = toLinqExpression_Many vars i_es
                let a_q     = toLinqExpression vars a_e
                let p_q     = Expression.Property (t_q, pi, i_qs)
                let as_q    = Expression.Assign (p_q, a_q)
                as_q :> Expression                
            | Quote                 q_q                 ->
                let c_q     = Expression.Constant q_q
                c_q :> Expression
            | Sequential            (f_e, s_e)          ->
                // TODO: Coalesce sequences
                let f_q     = toLinqExpression vars f_e
                let s_q     = toLinqExpression vars s_e
                let b_q     = Expression.Block (f_q, s_q)
                b_q :> Expression
            | Value                 (i,t)               ->
                let c_q     = Expression.Constant (i, t)
                c_q :> Expression
            | Var                   v                   ->
                let vd      = VariableDefinition.New v.Type v.Name
                let v_q     = vars |> Map.find vd
                v_q :> Expression
            | VarSet                (v,f)               ->
                let vd      = VariableDefinition.New v.Type v.Name
                let v_q     = vars |> Map.find vd
                let f_q     = toLinqExpression vars f
                let a_q     = Expression.Assign (v_q, f_q)
                a_q :> Expression
            | WhileLoop             (t_e, l_e)          ->
                let start       = Expression.Label ()   
                let test        = Expression.Label ()
                let startLabel_q= Expression.Label start    
                let testLabel_q = Expression.Label test     
                let gotoStart_q = Expression.Goto start     
                let gotoTest_q  = Expression.Goto test      

                let l_q         = toLinqExpression vars l_e
                let t_q         = toLinqExpression vars t_e
                let if_q        = Expression.IfThen (t_q, gotoStart_q)

                let b_q         = Expression.Block (gotoTest_q, startLabel_q, l_q, testLabel_q, if_q)
                b_q :> Expression
            | TupleGet              (t_e, idx)          ->
                let t_q         = toLinqExpression vars t_e
                let pi          = t_q.Type.GetProperty <| sprintf "Item%d" (idx + 1)
                let p_q         = Expression.Property (t_q, pi)
                p_q :> Expression
            | TryFinally            (b_e, f_e)          ->
                let b_q         = toLinqExpression vars b_e
                let f_q         = toLinqExpression vars f_e
                let tf_q        = Expression.TryFinally (b_q, f_q)
                tf_q :> Expression
            | TypeTest              (f_e, t)            ->
                let f_q         = toLinqExpression vars f_e
                let t_q         = Expression.TypeIs (f_q, t)
                t_q :> Expression
            // Difficult to realize with Linq Expressions
            | AddressOf             _ 
            | AddressSet            _
            // TODO:
            | LetRecursive          _
            | NewDelegate           _
            | NewRecord             _
            | NewUnionCase          _
            | TryWith               _                   
            | UnionCaseTest         _
            | _                     -> failwith "Unhandled expr: %A" expr
            

    let compile (expr : Expr<'T>) : unit->'T = 
        let rawExpr     = expr.Raw
        let mutable e   = Details.toLinqExpression Map.empty expr.Raw
        while e.CanReduce do
            e <- e.Reduce ()
        let resultType  = typeof<'T>
        if resultType = typeof<unit> then
            let l = Expression.Lambda<Action> (e)
            let c = l.Compile ()
            fun () -> c.Invoke (); Unchecked.defaultof<'T>
        else
            let l = Expression.Lambda<Func<'T>> (e)
            let c = l.Compile ()
            fun () -> c.Invoke ()
            

    

module QParser =
    open Microsoft.FSharp.Quotations
    open OptimizedClosures
    open QCompiler

    type CharStream<'UserState>(input : string, userState : 'UserState) =

        let mutable position            = 0
        let mutable noErrorMessages     = true

        let skipTemplate (charTest : Expr<char*int->bool>)= 
            <@
                let i           = input
                let length      = i.Length

                let mutable pos  = position
                let mutable iter = 0

                while pos < length && (%charTest) (i.[pos],iter) do
                    pos     <- pos + 1
                    iter    <- iter + 1

                position <- pos
                
                iter
            @>

        let skipTemplate2 (charTest : Expr<char->int->bool>)= 
            <@
                let i           = input
                let length      = i.Length

                let mutable pos  = position
                let mutable iter = 0

                while pos < length && (%charTest) i.[pos] iter do
                    pos     <- pos + 1
                    iter    <- iter + 1

                position <- pos
                
                iter
            @>

        let skipTemplate3 (charTest : Expr<char->int->string->bool>)= 
            <@
                let i           = input
                let length      = i.Length

                let mutable pos  = position
                let mutable iter = 0

                while pos < length && (%charTest) i.[pos] iter "" do
                    pos     <- pos + 1
                    iter    <- iter + 1

                position <- pos
                
                iter
            @>

        member x.StateTag               = position
        member x.Input                  = input
        member x.Position               = position
        member x.UserState              = userState
        member x.IsEndOfStream          = position >= input.Length || position < 0
        member x.NoErrorMessages        = noErrorMessages
        member x.Peek ()                = if x.IsEndOfStream then '\uFFFF' else input.[position]

        member x.SetPosition pos        = position <- pos
        member x.SetNoErrorMessages flag= noErrorMessages <- flag


        member x.MakeSkip (charTest : Expr<char*int->bool>) =
            skipTemplate charTest |> compile

        member x.MakeSkip2 (charTest : Expr<char->int->bool>) =
            skipTemplate2 charTest |> compile

        member x.MakeSkip3 (charTest : Expr<char->int->string->bool>) =
            skipTemplate3 charTest |> compile

open System.Diagnostics
open Microsoft.FSharp.Quotations

let timeIt what n action = 
    // Dry run
    ignore <| action ()

    printfn "Running %s %d times ..." what n
    let sw = Stopwatch ()
    sw.Start ()
    for i in 1..n do
        ignore <| action ()
    sw.Stop ()
    printfn "... took %d ms" sw.ElapsedMilliseconds

let qws1 : char*int->bool = 
    fun (ch,_) -> 
        match ch with
        | ' '
        | '\t'
        | '\n'
        | '\r'  -> true
        | _     -> false

let qws2 = qws1

let qws3 : char->int->bool = 
    fun ch _ -> 
        match ch with
        | ' '
        | '\t'
        | '\n'
        | '\r'  -> true
        | _     -> false

let qws4 = qws3

let qws5 : char->int->string->bool = 
    fun ch _ _ -> 
        match ch with
        | ' '
        | '\t'
        | '\n'
        | '\r'  -> true
        | _     -> false

let qws6 = qws5

let qwsQuote1 : Expr<char*int->bool> = 
    <@
        fun (ch,_) -> 
            match ch with
            | ' '
            | '\t'
            | '\n'
            | '\r'  -> true
            | _     -> false
    @>
let qwsQuote2 : Expr<char*int->bool> = 
    <@
        qws2
    @>
let qwsQuote3 : Expr<char->int->bool> = 
    <@
        fun ch _ -> 
            match ch with
            | ' '
            | '\t'
            | '\n'
            | '\r'  -> true
            | _     -> false
    @>
let qwsQuote4 : Expr<char->int->bool> = 
    <@
        qws4
    @>
let qwsQuote5 : Expr<char->int->string->bool> = 
    <@
        fun ch _ _-> 
            match ch with
            | ' '
            | '\t'
            | '\n'
            | '\r'  -> true
            | _     -> false
    @>
let qwsQuote6 : Expr<char->int->string->bool> = 
    <@
        qws6
    @>

[<EntryPoint>]
let main argv = 

    let t = typedefof<FSharpFunc<int,int>>
    
    let document    = System.String (' ', 10000)
    let n = 400

    let qcs = QParser.CharStream<unit> (document, ())
//    let skipper1 = qcs.MakeSkip qwsQuote2
//    let skipper2 = qcs.MakeSkip2 qwsQuote4
    let skipper3 = qcs.MakeSkip3 qwsQuote6
    //timeIt "Version4" n <| fun () -> qcs.SetPosition 0; skipper ()


    0
