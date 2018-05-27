module Galactus.Formlet.Core

open System
open System.Text.RegularExpressions

type [<Struct>] Maybe<'T> =
  | Just      of 'T
  | Nothing
and maybe<'T> = Maybe<'T>

module Maybe =
  let inline just v = Just v
  let nothing<'T>   = Nothing

type NotifyType = Submit | Reset | Change

type [<RequireQualifiedAccess; Struct>] FormletElementDecorator =
  | Attribute of string*string

type [<Struct>] FormletElementDecorators = 
  | FEDS of FormletElementDecorator list
  static member Empty = FEDS []

  member x.AppendAttribute key value =
    let (FEDS feds) = x
    FEDS (FormletElementDecorator.Attribute (key, value)::feds)


type FormletRenderTreeBuilder =
  interface
    abstract OpenElement      : tag:string -> feds:FormletElementDecorators -> unit
    abstract OpenValueElement : tag:string -> feds:FormletElementDecorators -> rval:string ref -> unit
    abstract AddAttribute     : key:string -> value:string -> unit
    abstract AddContent       : content:string -> unit
    abstract CloseElement     : unit -> unit
  end

type FormletContext = 
  {
    Builder     : FormletRenderTreeBuilder
    Notify      : Action<NotifyType>
  }
  static member New b n : FormletContext = { Builder = b; Notify = n }

type [<Struct>] FormletFailureContext =
  | FFC of (string list)
  static member Empty = FFC []

  member x.Append name : FormletFailureContext =
    let (FFC names) = x
    FFC (name::names)

[<NoComparison>]
type FormletTag (tag : string) =
  member x.Tag = tag

type [<RequireQualifiedAccess>] FormletTree =
  | Empty
  | Input         of FormletTag*string ref
  | Debug         of string*FormletTree
  | Named         of string*FormletTree
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
  | FL of (FormletContext -> FormletFailureContext -> FormletElementDecorators -> FormletTree -> FormletResult<'T>)
                  
module Details =
  let inline refEq l r = System.Object.ReferenceEquals (l, r)

  let inline fadapt (FL t) = OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt t

  let inline finvoke (t : OptimizedClosures.FSharpFunc<_, _, _, _, _>) fc ffc feds ft = t.Invoke (fc, ffc, feds, ft)

open Details

module Formlet =
  open FSharp.Core.Printf

  let value (v : 'T) : Formlet<'T> =
    FL <| fun fc ffc feds ft ->
      FR (v, FormletFailureTree.Empty, FormletTree.Empty)

  let failWith (fv : 'T) msg : Formlet<'T>  =
    FL <| fun fc ffc feds ft ->
      FR (fv, (FormletFailureTree.Failure (ffc, msg)), FormletTree.Empty)

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

  let apply (f: Formlet<'T -> 'U>) (t : Formlet<'T>) : Formlet<'U> =
    let ff = fadapt f
    let tf = fadapt t
    FL <| fun fc ffc feds ft ->

      let fft, tft =
        match ft with
        | FormletTree.Fork (fft, tft) -> fft              , tft
        | _                           -> FormletTree.Empty, FormletTree.Empty

      let (FR (ff, ffft, fft)) = finvoke ff fc ffc feds fft
      let (FR (tv, tfft, tft)) = finvoke tf fc ffc feds tft

      FR (ff tv, FormletFailureTree.Join ffft tfft, FormletTree.Fork (fft, tft))

  let map m (t : Formlet<'T>) : Formlet<'U> =
    let tf = fadapt t
    FL <| fun fc ffc feds ft ->

      let (FR (tv, tfft, tft)) = finvoke tf fc ffc feds ft

      FR (m tv, tfft, tft)

  let andAlso (t: Formlet<'T>) (u : Formlet<'U>) : Formlet<'T*'U> =
    let tf = fadapt t
    let uf = fadapt u
    FL <| fun fc ffc feds ft ->

      let tft, uft =
        match ft with
        | FormletTree.Fork (tft, uft) -> tft              , uft
        | _                           -> FormletTree.Empty, FormletTree.Empty

      let (FR (tv, tfft, tft)) = finvoke tf fc ffc feds tft
      let (FR (uv, ufft, uft)) = finvoke uf fc ffc feds uft

      FR ((tv, uv), FormletFailureTree.Join tfft ufft, FormletTree.Fork (tft, uft))

  let unwrap (t : Formlet<Formlet<'T>>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc feds ft ->

      let tft, uft =
        match ft with
        | FormletTree.Fork (tft, uft) -> tft              , uft
        | _                           -> FormletTree.Empty, FormletTree.Empty

      let (FR (u, tfft, tft)) = finvoke tf fc ffc feds tft

      let uf = fadapt u

      let (FR (uv, ufft, uft)) = finvoke uf fc ffc feds uft

      FR (uv, FormletFailureTree.Join tfft ufft, FormletTree.Fork (tft, uft))

  let debug name (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc feds ft ->

      let ft =
        match ft with
        | FormletTree.Debug (_, sft)    -> sft
        | _                             -> ft

      let (FR (tv, tfft, tft)) = finvoke tf fc ffc feds ft

      FR (tv, tfft, FormletTree.Debug (name, tft))

  let named name (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc feds ft ->

      let ft =
        match ft with
        | FormletTree.Named (snm, sft) when snm.Equals name -> sft
        | _                                                 -> FormletTree.Empty

      let (FR (tv, tfft, tft)) = finvoke tf fc ffc feds ft

      FR (tv, tfft, FormletTree.Named (name, tft))

  let buildUp builder notify t ft =
    let fc    = FormletContext.New builder notify
    let ffc   = FormletFailureContext.Empty
    let feds  = FormletElementDecorators.Empty
    let t     = fadapt t
    finvoke t fc ffc feds ft

  type Builder () =
    member inline x.Bind       (t, uf)   = bind t uf
    member inline x.Return     v         = value v
    member inline x.ReturnFrom t         = t         : Formlet<_>
    member inline x.Zero       ()        = value ()

let formlet = Formlet.Builder ()

type Formlet<'T> with
  static member inline (>>=) (t, uf)  = Formlet.bind    t uf
  static member inline (|>>) (t, m)   = Formlet.map     m t
  static member inline (<*>) (f, t)   = Formlet.apply   f t
  static member inline (<&>) (l, r)   = Formlet.andAlso l r

module Validate =
  let validate (validator : 'T -> string maybe) (t : Formlet<'T>) : Formlet<'T> =
    let tf = fadapt t
    FL <| fun fc ffc feds ft ->

      let tfr = finvoke tf fc ffc feds ft
      let (FR (tv, tfft, tft)) = tfr

      match validator tv with
      | Just failure  ->
        FR (tv, FormletFailureTree.Join tfft (FormletFailureTree.Failure (ffc, failure)), tft)
      | Nothing       ->
        tfr

  let notEmpty (t : Formlet<string>) : Formlet<string> =
    let validator (v : string) =
      if v.Length > 0 then
        Nothing
      else
        Just "Must not be empty"
    validate validator t

  let regex (test : Regex) failWith (t : Formlet<string>) : Formlet<string> =
    let validator (v : string) =
      if test.IsMatch v then
        Nothing
      else
        Just failWith
    validate validator t

module EnhanceWith =
  let attribute key value t : Formlet<'T> =
    let t = fadapt t
    FL <| fun fc ffc feds ft ->
      finvoke t fc ffc (feds.AppendAttribute key value) ft

  let label label for_ t : Formlet<'T> =
    let t = fadapt t
    FL <| fun fc ffc feds ft ->
      fc.Builder.OpenElement "label" FormletElementDecorators.Empty
      fc.Builder.AddAttribute "for" for_
      fc.Builder.AddContent label
      fc.Builder.CloseElement ()

      finvoke t fc ffc feds ft

module Tag =
  module Details =
    let inputTag = FormletTag "input"
  open Details

  let tag tag t : Formlet<'T> =
    let t = fadapt t
    FL <| fun fc ffc feds ft ->
      fc.Builder.OpenElement tag feds
      let tr = finvoke t fc ffc FormletElementDecorators.Empty ft
      fc.Builder.CloseElement ()
      tr

  let input placeholder initial : Formlet<string> =
    FL <| fun fc ffc feds ft ->
      let rval =
        match ft with
        | FormletTree.Input (tag, rval) when refEq inputTag tag ->
          rval
        | _ ->
          ref initial

      fc.Builder.OpenValueElement "input" feds rval
      fc.Builder.AddAttribute "placeholder" placeholder
      fc.Builder.AddAttribute "type" "text"
      fc.Builder.CloseElement ()
      
      let v = !rval

      FR (v, FormletFailureTree.Empty, FormletTree.Input (inputTag, rval))

module Test =
  let test =
    formlet {
      let! firstName  = Tag.input "First name" ""
      let! lastName   = Tag.input "Last name" ""
      return firstName, lastName
    }
