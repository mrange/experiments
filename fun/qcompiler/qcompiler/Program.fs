
module QCompiler = 
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open System
    open System.Linq.Expressions

    module Details = 
        let gdt :Type   = typedefof<Converter>
        let gfft:Type   = typedefof<FSharpFunc>

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


        let optimizeApplication (expr : Expr) = 
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
        and toLinqExpression (vars : VariableExpressions) (expr : Expr) : Expression = 
            match expr with
            | Application           (f_e,a_e)           ->
                let o_e = optimizeApplication expr
                null :> Expression
                // TODO:
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
//            | Lambda                (v,e_e)             ->
//                let p_q     = Expression.Parameter (v.Type, v.Name)
//                let e_q     = toLinqExpression vars e_e
//                let dt      = gdt.MakeGenericType (p_q.Type, e_q.Type)                
//                let l_q     = Expression.Lambda (dt,e_q,p_q)
//                let fft     = gfft.Make (p_q.Type, e_q.Type)
//                FSharpFunc.

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
            | TryFinally            (b_e, f_e)          ->
                let b_q         = toLinqExpression vars b_e
                let f_q         = toLinqExpression vars f_e
                let tf_q        = Expression.TryFinally (b_q, f_q)
                tf_q :> Expression
            | TypeTest              (f_e, t)            ->
                let f_q         = toLinqExpression vars f_e
                let t_q         = Expression.TypeIs (f_q, t)
                t_q :> Expression
            //  
            | AddressOf             _ 
            | AddressSet            _

            | LetRecursive          _
            | NewDelegate           _
            | NewRecord             _
            | NewTuple              _
            | NewUnionCase          _
            | TryWith               _                   
            | TupleGet              _
            | UnionCaseTest         _
            | _                     -> failwith "Unhandled case"
            

    let compile (expr : Expr<'T>) : unit->'T = 
        let e = Details.toLinqExpression Map.empty expr.Raw
        let l = Expression.Lambda<Func<unit, 'T>> (e)
        let c = l.Compile ()
        fun () -> c.Invoke ()

    

module QParser =
    open Microsoft.FSharp.Quotations
    open OptimizedClosures
    open QCompiler

    type CharStream<'UserState>(input : string, userState : 'UserState) =

        let mutable position            = 0
        let mutable noErrorMessages     = true

        let skipTemplate (charTest : Expr<char->int->bool>)= 
            <@
                let i           = input
                let length      = i.Length

                let mutable pos  = position
                let mutable iter = 0

                while pos < length && (%charTest) i.[pos] iter do
                    pos     <- pos + 1
                    iter    <- iter + 1

                position <- pos
                
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


        member x.MakeSkip (charTest : Expr<char->int->bool>) =
            skipTemplate charTest |> compile

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

let qwsTest : Expr<char->int->bool> = 
    <@
        fun ch _ -> 
            match ch with
            | ' '
            | '\t'
            | '\n'
            | '\r'  -> true
            | _     -> false
    @>

[<EntryPoint>]
let main argv = 

    let x : FSharpFunc<int, int>= FSharpFunc<int,int>( 

    let document    = System.String (' ', 10000)
    let n = 400

    let qcs = QParser.CharStream<unit> (document, ())
    let skipper = qcs.MakeSkip qwsTest
    timeIt "Version4" n <| fun () -> qcs.SetPosition 0; skipper ()


    0
