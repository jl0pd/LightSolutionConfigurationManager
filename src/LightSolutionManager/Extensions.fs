namespace LightSolutionManager

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

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    let vChoosei (map: int -> 'a -> 'b voption) (input: 'a list) =
        let rec inner i acc = function
            | [] -> acc
            | x :: xs ->
                match map i x with
                | ValueSome v -> inner (i + 1) (v :: acc) xs
                | ValueNone -> inner (i + 1) acc xs
        inner 0 [] input |> List.rev

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module KeyValue =
    open System.Collections.Generic

    let value (kvp: KeyValuePair<_,_>) = kvp.Value
    let key (kvp: KeyValuePair<_,_>) = kvp.Key

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Regex =
    open System
    open System.Text.RegularExpressions

    let isMatch input pattern =
        try
            Regex.IsMatch(input, pattern)
        with
        | :? ArgumentException -> false

    let isMatchCaseInsensitive input pattern =
        try
            Regex.IsMatch(input, pattern, RegexOptions.IgnoreCase)
        with
        | :? ArgumentException -> false
