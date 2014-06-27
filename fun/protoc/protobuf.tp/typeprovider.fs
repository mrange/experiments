namespace Protobuf.TypeProvider

open Microsoft.FSharp.Core.CompilerServices 
open Microsoft.FSharp.Quotations

open System
open System.IO
open System.Reflection

[<assembly: TypeProviderAssembly>]
do ()

type ProtobufProvider () =
    inherit obj()

[<AutoOpen>]
module internal ProtobufTypeProviderAutoOpen =
    type StringParameterInfo (name : string) =
        inherit ParameterInfo ()

        override this.Name              with get() = name
        override this.ParameterType     with get() = typeof<string>
        override this.Position          with get() = 0
        override this.RawDefaultValue   with get() = upcast ""
        override this.DefaultValue      with get() = upcast ""
        override this.Attributes        with get() = ParameterAttributes.None

[<TypeProvider>]
type ProtobufTypeProvider() =

    let invalidation = new Event<EventHandler, EventArgs>()

    interface ITypeProvider with
        member this.ApplyStaticArguments (typeWithoutArguments, typeNameWithArguments, staticArguments) = 
                null
        member this.GetGeneratedAssemblyContents assembly = 
                let bytes = File.ReadAllBytes assembly.ManifestModule.FullyQualifiedName
                bytes
        member this.GetInvokerExpression (syntheticMethodBase, parameters) =
                  match syntheticMethodBase with
                  | :? ConstructorInfo as ctor -> Expr.NewObject (ctor, parameters |> List.ofArray)
                  | :? MethodInfo as mi -> Expr.Call(parameters.[0], mi, parameters |> List.ofArray)
                  | _ -> let pnames = 
                            parameters 
                         failwith <| sprintf "ERROR: Don't know what to do in GetInvokerExpression - %s" syntheticMethodBase.Name
        member this.GetNamespaces () = 
                [| this |]
        member this.GetStaticParameters typeWithoutArguments =
                [| new StringParameterInfo ("protobufSpecification") |]
        [<CLIEvent>]
        member this.Invalidate = invalidation.Publish
        member this.Dispose() = ()

    interface IProvidedNamespace with
        member this.GetNestedNamespaces () = 
                [| |]
        member this.GetTypes () = 
                [| typeof<ProtobufProvider> |]
        member this.ResolveTypeName typeName = 
                typeof<ProtobufProvider>

        member this.NamespaceName with get() = "Protobuf.Messages"


