namespace LightSolutionManager.Extensions

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    let skip count (str: string) =
        str.Substring count

    let take count (str: string) =
        str.Substring(0, count)

    let skipLast count (str: string) =
        str.Substring(0, str.Length - count)

    [<RequireQualifiedAccess>]
    module Invariant =
        let compare left right =
            String.Compare(left, right, StringComparison.InvariantCulture)

        let compareIgnoreCase left right =
            String.Compare(left, right, StringComparison.InvariantCultureIgnoreCase)

        let equals left right =
            String.Equals(left, right, StringComparison.InvariantCulture)

        let equalsIgnoreCase left right =
            String.Equals(left, right, StringComparison.InvariantCultureIgnoreCase)

    [<RequireQualifiedAccess>]
    module Ordinal =
        let compare left right =
            String.Compare(left, right, StringComparison.Ordinal)

        let compareIgnoreCase left right =
            String.Compare(left, right, StringComparison.OrdinalIgnoreCase)

        let equals left right =
            String.Equals(left, right, StringComparison.Ordinal)

        let equalsIgnoreCase left right =
            String.Equals(left, right, StringComparison.OrdinalIgnoreCase)

        let endsWith subStr (str: string) =
            str.EndsWith(subStr, StringComparison.Ordinal)
