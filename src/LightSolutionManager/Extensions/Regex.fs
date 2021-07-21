namespace LightSolutionManager.Extensions

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
