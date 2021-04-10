namespace LightSolutionConfigurationManager

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    let partition predicate source =
        let map =
            source
                |> Seq.groupBy predicate
                |> Map.ofSeq
        let get flag =
            map
                |> Map.tryFind flag
                |> Option.defaultValue Seq.empty
        get true, get false
