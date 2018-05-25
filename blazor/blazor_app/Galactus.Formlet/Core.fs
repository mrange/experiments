namespace Galactus.Formlet.Core

type [<Struct>] Maybe<'T> =
  | Just      of 'T
  | Nothing
and maybe<'T> = Maybe<'T>

module Maybe =
  let inline just v = Just v
  let nothing<'T>   = Nothing

type NotifyType = Submit | Reset | Change

type FormletRenderTreeBuilder = FRTB

type [<Struct>] FormletElementDecorator = FED

type FormletContext = 
  {
    Attributes  : struct (string*string) []
    Builder     : FormletRenderTreeBuilder
    Notify      : NotifyType -> unit
  }

  member x.OpenElement tag feds : unit =
    ()

  member x.OpenValueElement tag feds rval : unit =
    ()

  member x.AddAttribute k v : unit =
    ()

  member x.CloseElement () : unit =
    ()

type [<Struct>] FormletFailureContext =
  | FFC of (string list)

  with
    member x.Append name : FormletFailureContext =
      let (FFC names) = x
      FFC (name::names)

type [<RequireQualifiedAccess>] FormletTree =
  | Empty
  | Input         of obj*string ref
  | Debug         of string*FormletTree
  | Tag           of string*FormletTree
  | Fork          of FormletTree*FormletTree

type [<RequireQualifiedAccess>] FormletFailureTree =
  | Empty
  | Failure of FormletFailureContext*string
  | Fork    of FormletFailureTree*FormletFailureTree

  static member Join l r : FormletFailureTree =
    match l, r with
    | Empty , Empty -> Empty
    | _     , Empty -> l
    | Empty , _     -> r
    | _     , _     -> Fork (l, r)

  member x.ContextfulFailures () : struct (string*string) [] =
    let ra = ResizeArray 16
    // TODO: Optimize
    let toContext (FFC vs) =
      System.String.Join (".", vs |> List.rev |> List.toArray)
    let rec loop t =
      match t with
      | Empty -> ()
      | Failure (ffc, msg)  -> ra.Add (struct (toContext ffc, msg))
      | Fork    (l, r)      -> loop l; loop r
    loop x
    ra.ToArray ()

  member x.Failures () : string [] =
    let ra = ResizeArray 16
    let rec loop t =
      match t with
      | Empty -> ()
      | Failure (ffc, msg)  -> ra.Add msg
      | Fork    (l, r)      -> loop l; loop r
    loop x
    ra.ToArray ()

type [<Struct>] FormletResult<'T> = FR of 'T*FormletFailureTree*FormletTree

type [<Struct>] Formlet<'T> =
  | FL of (FormletContext -> FormletFailureContext -> FormletElementDecorator list -> FormletTree -> FormletResult<'T>)

module Details =
  let inline refEq l r = System.Object.ReferenceEquals (l, r)

  let inline fadapt (FL t) = OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt t

  let inline finvoke (t : OptimizedClosures.FSharpFunc<_, _, _, _, _>) fc ffc feds ft = t.Invoke (fc, ffc, feds, ft)

  let inline fresult v fft ft = FR (v, fft, ft)

  let tag name =
    { new obj () with
        override x.ToString () = name
    }
open Details

module Formlet =
  open FSharp.Core.Printf

  let value (v : 'T) : Formlet<'T> =
    FL <| fun fc ffc feds ft ->
      fresult v FormletFailureTree.Empty FormletTree.Empty

  let failWith (fv : 'T) msg : Formlet<'T>  =
    FL <| fun fc ffc feds ft ->
      fresult fv (FormletFailureTree.Failure (ffc, msg)) FormletTree.Empty

  let failWithf fv fmt = kprintf (failWith fv) fmt

  let bind (t : Formlet<'T>) (uf : 'T -> Formlet<'U>) : Formlet<'U> =
    let tf = fadapt t
    FL <| fun fc ffc feds ft ->

      let tft, uft =
        match ft with
        | FormletTree.Fork (tft, uft) -> tft              , uft
        | _                           -> FormletTree.Empty, FormletTree.Empty

      let (FR (tv, tfft, tft)) = finvoke tf fc ffc feds tft

      let u  = uf tv
      let uf = fadapt u

      let (FR (uv, ufft, uft)) = finvoke uf fc ffc feds uft

      FR (uv, FormletFailureTree.Join tfft ufft, FormletTree.Fork (tft, uft))

module Inputs =
  module Tags =
    let input = obj ()

  let input placeholder initial : Formlet<string> =
    FL <| fun fc ffc feds ft ->
      let rval =
        match ft with
        | FormletTree.Input (tag, rval) when refEq Tags.input tag ->
          rval
        | _ ->
          ref initial

      fc.OpenValueElement "input" feds rval
      fc.AddAttribute "placeholder" placeholder
      fc.AddAttribute "type" "text"
      fc.CloseElement ()
      
      let v = !rval

      FR (v, FormletFailureTree.Empty, FormletTree.Input (Tags.input, rval))
