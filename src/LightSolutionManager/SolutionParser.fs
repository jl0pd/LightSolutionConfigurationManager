module LightSolutionManager.SolutionParser

open System
open System.Text
open FSharp.NativeInterop
open System.Globalization
open FParsec
open LightSolutionManager.Collections

let private manyNSatisfy n p =
    manyMinMaxSatisfy n n p

#nowarn "9"

let private pGuid =
    // {6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}
    between
        (pchar '{')
        (pchar '}')
        (pipe5
            (manyNSatisfy 8 isHex .>> skipChar '-')
            (manyNSatisfy 4 isHex .>> skipChar '-')
            (manyNSatisfy 4 isHex .>> skipChar '-')
            (manyNSatisfy 4 isHex .>> skipChar '-')
            (manyNSatisfy 12 isHex)
            (fun a b c d e ->
                let stackPtr = NativePtr.toVoidPtr (NativePtr.stackalloc<byte> 16)
                let span = Span(stackPtr, 16)

                for i = 0 to 3 do
                    span.[3 - i] <- Byte.Parse(a.AsSpan(i * 2, 2), NumberStyles.HexNumber)

                for i = 0 to 1 do
                    span.[5 - i] <- Byte.Parse(b.AsSpan(i * 2, 2), NumberStyles.HexNumber)

                for i = 0 to 1 do
                    span.[7 - i] <- Byte.Parse(c.AsSpan(i * 2, 2), NumberStyles.HexNumber)

                for i = 0 to 1 do
                    span.[i + 8] <- Byte.Parse(d.AsSpan(i * 2, 2), NumberStyles.HexNumber)

                for i = 0 to 5 do
                    span.[10 + i] <- Byte.Parse(e.AsSpan(i * 2, 2), NumberStyles.HexNumber)

                Guid(ReadOnlySpan(stackPtr, 16)))
        )

type private HeaderInfo =
    { FormatVersion : Version
      VSVersion : Version
      MinVSVersion : Version }

let private pVersion =
    sepBy pint32 (pchar '.')
    |>> (function
         | [x; y] -> Version(x, y)
         | [x; y; z] -> Version(x, y, z)
         | [x; y; z; w] -> Version(x, y, z, w)
         | v -> raise (FormatException $"Invalid version specifier parsed into %A{v}"))

let private pFormatVersion =
    skipString "Microsoft Visual Studio Solution File, Format Version " >>. pVersion

let private pVisualStudioVersion =
    skipString "VisualStudioVersion = " >>. pVersion

let private pMinimalVisualStudioVersion =
    skipString "MinimumVisualStudioVersion = " >>. pVersion

let private pComment =
    many1Satisfy2 ((=) '#') ((<>) '\n') .>> spaces

let private manyComments = many pComment

let private spacesOrComments = spaces >>. manyComments

let private pFileHeader =
    spacesOrComments >>.
        pipe3
            (pFormatVersion .>> spacesOrComments)
            (pVisualStudioVersion .>> spacesOrComments)
            (pMinimalVisualStudioVersion .>> spacesOrComments)
            (fun fv vsv mvsv -> { FormatVersion = fv; VSVersion = vsv; MinVSVersion = mvsv })

type private ProjectHeader =
    { Type : Guid
      Name : string
      Path : Uri
      Id : Guid }

let private doubleQuote = pchar '"'

let private betweenDoubleQuotes p =
    between doubleQuote doubleQuote p

let private doubleQuotedString =
    betweenDoubleQuotes (manyChars (noneOf "\""))

let private parenthessedString =
    between (pchar '(') (pchar ')') (manyChars (noneOf ")"))

let private pProjectHeader =
    pipe4
        (skipString "Project" >>. between (skipString "(\"") (skipString "\")") pGuid)
        (skipString " = " >>. doubleQuotedString)
        (skipString ", " >>. doubleQuotedString)
        (skipString ", " >>. betweenDoubleQuotes pGuid)
        (fun t n p i -> { Type = t; Name = n; Path = Uri(p, UriKind.RelativeOrAbsolute); Id = i })
    .>> spacesOrComments

type private SectionKind =
    | PreProject
    | PostProject
    | PreSolution
    | PostSolution

type SectionKind with
    static member Parse input =
        if "preProject".Equals(input, StringComparison.InvariantCultureIgnoreCase) then
            PreProject
        elif "PostProject".Equals(input, StringComparison.InvariantCultureIgnoreCase) then
            PostProject
        elif "PreSolution".Equals(input, StringComparison.InvariantCultureIgnoreCase) then
            PreSolution
        elif "PostSolution".Equals(input, StringComparison.InvariantCultureIgnoreCase) then
            PostSolution
        else
            invalidArg (nameof(input)) $"Unmatched name '%s{input}'"

type private SectionDefinition =
    { Name : string
      Kind : SectionKind
      Items : Map<string, string> }

let private pPair =
    (pipe2
        (many1Chars (noneOf "=") .>> skipChar '=')
        (many1Chars (noneOf "\n") .>> spacesOrComments)
        (fun f s ->
            let f = f.Trim()
            let s = s.Trim()
            (f, s)))

let private pSection start stop =
    pipe3
        (skipString start >>. parenthessedString .>> skipString " = ")
        (many1Satisfy isLetter .>> spacesOrComments)
        (manyTill
            pPair
            (skipString stop .>> spacesOrComments))
        (fun n k i -> { Name = n; Kind = SectionKind.Parse k; Items = Map.ofList i })

let private pProjectSection =
    pSection "ProjectSection" "EndProjectSection"

type private ProjectDefinition =
    { Name : string
      Type : Guid
      Path : Uri
      Id : Guid
      Sections : Map<string, SectionDefinition> }

let private pProject =
    pipe2
        pProjectHeader
        (many pProjectSection .>> (skipString "EndProject") .>> spacesOrComments)
        (fun h s -> { Name = h.Name
                      Type = h.Type
                      Path = h.Path
                      Id = h.Id
                      Sections = s |> List.map (fun sc -> (sc.Name, sc)) |> Map.ofList })

let private manyProjects =
    many (pProject .>> spacesOrComments)

let private pGlobalSection =
    pSection "GlobalSection" "EndGlobalSection"

let private pGlobal =
    between
        (skipString "Global" .>> spacesOrComments)
        (skipString "EndGlobal" .>> spacesOrComments)
        (many pGlobalSection .>> spacesOrComments)

type private SolutionDefinition =
    { Info : HeaderInfo
      Projects : ProjectDefinition list
      Globals : Map<string, SectionDefinition> }

let private pSolution =
    pipe3
        pFileHeader
        manyProjects
        pGlobal
        (fun h p g -> { Info = h
                        Projects = p
                        Globals = g |> List.map (fun s -> (s.Name, s)) |> Map.ofList } )

[<RequireQualifiedAccess>]
module private Map =
    let keys (map: Map<_,_>) =
        (map :> System.Collections.Generic.IDictionary<_, _>).Keys

type private ProjectCfgMapping =
    { ProjectId : Guid
      Configuration : SolutionConfiguration
      Build : bool
      BuildConfiguration : SolutionConfiguration }

let private parseSolutionConfiguration (s: string) =
    match s.Split '|' with
    | [| cfg; platform |] -> { Configuration = cfg; Platform = platform }
    | _ -> invalidArg (nameof(s)) $"Incorrect configuration-platform pair '%s{s}'"

/// Parses given file using UTF-8 encoding
let parseFile (path: string) : Solution =
    let parseResult = runParserOnFile pSolution () path Encoding.UTF8

    match parseResult with
    | Failure (e, _, _) -> raise (System.IO.InvalidDataException e)
    | Success ({ Info = info; Globals = globals; Projects = projects }, _, _) -> // TODO: split to 2 different functions
        let cfgs = globals.["SolutionConfigurationPlatforms"].Items
                   |> Map.keys
                   |> Seq.map parseSolutionConfiguration
                   |> Seq.toList

        let cfgMapping = globals.["ProjectConfigurationPlatforms"].Items
                         |> Map.toArray
                         |> Array.map (fun (k, v) ->
                                let buildCfg = parseSolutionConfiguration v
                                match k.Split '.' with
                                | [| id; cfg; "ActiveCfg" |] -> { ProjectId = Guid.Parse id
                                                                  Configuration = parseSolutionConfiguration cfg
                                                                  Build = false
                                                                  BuildConfiguration = buildCfg }
                                | [| id; cfg; "Build"; "0" |] -> { ProjectId = Guid.Parse id
                                                                   Configuration = parseSolutionConfiguration cfg
                                                                   Build = true
                                                                   BuildConfiguration = buildCfg }
                                | _ -> invalidArg (nameof(k)) $"Invalid mapping '%s{k}'")
                         |> Array.groupBy (fun p -> (p.ProjectId, p.Configuration))
                         |> Array.map (fun ((id, cfg), mappings) ->
                                match mappings with
                                | [| x |] -> ((id, cfg), x)
                                | [| x; y |] -> ((id, cfg), { ProjectId = id
                                                              Configuration = cfg
                                                              Build = true
                                                              BuildConfiguration = x.BuildConfiguration })
                                | _ -> invalidArg (nameof(mappings)) $"Invalid mappings length '%d{mappings.Length}'")
                         |> Map.ofArray

        let nestedProjects = Map.tryFind "NestedProjects" globals
                             |> Option.map (fun sec ->
                                    sec.Items
                                    |> Map.toArray
                                    |> Array.map (fun (k, v) -> (Guid k, Guid v))
                                    |> Map.ofArray)

        let projectsInOrder =
            projects
            |> List.map (fun p ->
                let projCfgs = cfgMapping
                               |> Map.toArray
                               |> Array.choose (fun ((id, slnCfg), projCfg) ->
                                    if id = p.Id then
                                        Some (slnCfg, { Configuration = projCfg.BuildConfiguration
                                                        IncludeInBuild = projCfg.Build })
                                    else None)
                               |> Map.ofArray

                let parentId = nestedProjects
                               |> Option.bind (Map.tryFind p.Id)

                let folderFiles = Map.tryFind "SolutionItems" p.Sections
                                  |> Option.map (fun sec -> Map.keys sec.Items |> List.ofSeq)
                                  |> Option.defaultValue List.empty

                let dependencies = Map.tryFind "ProjectDependencies" p.Sections
                                   |> Option.map (fun sec -> Map.keys sec.Items
                                                             |> Seq.map Guid
                                                             |> List.ofSeq)
                                   |> Option.defaultValue List.empty

                let proj = { Id = p.Id
                             TypeId = p.Type
                             Name = p.Name
                             Path = p.Path
                             ParentId = parentId
                             FolderFiles = folderFiles
                             Dependencies = dependencies
                             Configurations = projCfgs }
                proj)

        let nestedProjectsInOrder = nestedProjects
                                    |> Option.map (Map.keys >> Seq.toList)
                                    |> Option.defaultValue List.empty

        let properties = Map.tryFind "SolutionProperties" globals
                         |> Option.map (fun s -> s.Items)
                         |> Option.defaultValue Map.empty

        let slnId = Map.tryFind "ExtensibilityGlobals" globals
                    |> Option.bind (fun s -> Map.tryFind "SolutionGuid" s.Items)
                    |> Option.map Guid
                    |> Option.defaultValue Unchecked.defaultof<Guid>

        let sln = { FormatVersion = info.FormatVersion
                    VisualStudioVersion = info.VSVersion
                    MinimumVisualStudioVersion = info.MinVSVersion
                    Configurations = cfgs
                    ProjectsInOrder = OrderedMap.ofSeq (projectsInOrder |> Seq.map (fun p -> (p.Id, p)) )
                    NestedProjectsInOrder = nestedProjectsInOrder
                    Properties = properties
                    FileName = Some path
                    Id = slnId }
        sln
