namespace LightSolutionManager

open System
open System.Diagnostics
open Microsoft.FSharp.Core.Printf


[<DebuggerDisplay("{FullName}")>]
type SolutionConfiguration =
    { Configuration: string
      Platform: string }

[<RequireQualifiedAccess>]
module SolutionConfiguration =
    let fullName ({ Configuration = c; Platform = p } as cfg) = // cfg for readable name in generated code, don't remove
        if String.IsNullOrEmpty p then
            c
        else
            sprintf "%s|%s" c p

type SolutionConfiguration with
    member c.FullName = SolutionConfiguration.fullName c


[<DebuggerDisplay("{GetDebuggerDisplay(),nq}")>]
type ProjectConfiguration =
    { Configuration: SolutionConfiguration
      IncludeInBuild: bool }

[<RequireQualifiedAccess>]
module ProjectConfiguration =
    let fullName c =
        SolutionConfiguration.fullName c.Configuration

    let configuration cfg =
        cfg.Configuration

type ProjectConfiguration with
    member c.FullName = ProjectConfiguration.fullName c
    member private c.GetDebuggerDisplay () =
        let build = if c.IncludeInBuild then "+" else "-"
        $"%s{c.Configuration.FullName}: build %s{build}"


type Project =
    { Id: Guid
      TypeId: Guid
      ParentId: Guid option
      Name: string
      Path: Uri
      FolderFiles: string list
      Dependencies: Guid list
      Configurations: Map<SolutionConfiguration, ProjectConfiguration> }

[<RequireQualifiedAccess>]
module Project =

    let folderGuid = Guid "{2150E333-8FDC-42A3-9474-1A3956D46DE8}"

    let isFolder project =
        project.TypeId = folderGuid

type Project with
    member p.IsFolder =
        Project.isFolder p


type Solution =
    { FormatVersion: Version
      VisualStudioVersion: Version
      MinimumVisualStudioVersion: Version
      Configurations: SolutionConfiguration list
      ProjectsInOrder: Project list
      NestedProjectsInOrder: Guid list
      Properties: Map<string, string>
      Id: Guid }

[<RequireQualifiedAccess>]
module Solution =
    let projectsByGuid sln =
        sln.ProjectsInOrder
        |> List.fold (fun state p -> Map.add p.Id p state) Map.empty

    let private guidToString (g: Guid) =
        g.ToString("B").ToUpper()

    let private saveHeader writeLine sln =
        let v = $"Microsoft Visual Studio Solution File, Format Version %s{sln.FormatVersion.ToString 1}.00"
        let vsVm = $"# Visual Studio Version %d{sln.VisualStudioVersion.Major}"
        let vsV = $"VisualStudioVersion = %O{sln.VisualStudioVersion}"
        let mV = $"MinimumVisualStudioVersion = %O{sln.MinimumVisualStudioVersion}"
        writeLine v
        writeLine vsVm
        writeLine vsV
        writeLine mV

    let private saveProject writeLine project =
        writeLine $"Project(\"%s{guidToString project.TypeId}\") = \"%s{project.Name}\", \"%O{project.Path}\", \"%s{guidToString project.Id}\""

        if not project.FolderFiles.IsEmpty then
            writeLine "\tProjectSection(SolutionItems) = preProject"
            for file in project.FolderFiles do
                writeLine $"\t\t%s{file} = %s{file}"
            writeLine "\tEndProjectSection"

        if not project.Dependencies.IsEmpty then
            writeLine "\tProjectSection(ProjectDependencies) = postProject"
            for dep in project.Dependencies do
                writeLine $"\t\t%s{guidToString dep} = %s{guidToString dep}"
            writeLine "\tEndProjectSection"

        writeLine "EndProject"

    let private saveProjects writeLine projects =
        for project in projects do
            saveProject writeLine project

    let private saveSolutionConfigurations writeLine (configurations: SolutionConfiguration seq) =
        writeLine "\tGlobalSection(SolutionConfigurationPlatforms) = preSolution"

        for cfg in configurations do
            writeLine $"\t\t%s{cfg.FullName} = %s{cfg.FullName}"

        writeLine "\tEndGlobalSection"

    let private saveProjectConfigurations writeLine (slnConfigurations: SolutionConfiguration seq) (projects: Project seq) =
        writeLine "\tGlobalSection(ProjectConfigurationPlatforms) = postSolution"

        for proj in projects do
            for slnCfg in slnConfigurations do
                match proj.Configurations.TryGetValue slnCfg with
                | (true, cfg) ->
                    let sc = slnCfg.FullName
                    writeLine $"\t\t%s{guidToString proj.Id}.%s{sc}.ActiveCfg = %s{cfg.FullName}"
                    if cfg.IncludeInBuild then
                        writeLine $"\t\t%s{guidToString proj.Id}.%s{sc}.Build.0 = %s{cfg.FullName}"
                | (false, _) -> ()

        writeLine "\tEndGlobalSection"

    let private saveSolutionProperties writeLine =
        writeLine "\tGlobalSection(SolutionProperties) = preSolution"
        writeLine "\t\tHideSolutionNode = FALSE"
        writeLine "\tEndGlobalSection"

    let private saveNestedProjects writeLine (projectsByGuid: Map<Guid, Project>) (projects: Guid list) =
        if not projects.IsEmpty then
            writeLine "\tGlobalSection(NestedProjects) = preSolution"

            for guid in projects do
                let proj = projectsByGuid.[guid]
                match proj.ParentId with
                | Some g -> writeLine $"\t\t%s{guidToString proj.Id} = %s{guidToString g}"
                | None -> ()

            writeLine "\tEndGlobalSection"

    let private saveExtensibility writeLine sln =
        writeLine "\tGlobalSection(ExtensibilityGlobals) = postSolution"
        writeLine $"\t\tSolutionGuid = %s{guidToString sln.Id}"
        writeLine "\tEndGlobalSection"

    let private saveGlobal writeLine sln =
        writeLine "Global"

        saveSolutionConfigurations writeLine sln.Configurations
        saveProjectConfigurations writeLine sln.Configurations sln.ProjectsInOrder
        saveSolutionProperties writeLine
        saveNestedProjects writeLine (projectsByGuid sln) sln.NestedProjectsInOrder
        saveExtensibility writeLine sln

        writeLine "EndGlobal"

    let saveTo writeLine sln =
        writeLine ""
        saveHeader writeLine sln
        saveProjects writeLine sln.ProjectsInOrder
        saveGlobal writeLine sln

    let saveToStringBuilder sln =
        let sb = Text.StringBuilder()
        saveTo (sb.AppendLine >> ignore) sln
        sb

    let saveToFile path sln =
        use file = IO.File.Open(path, IO.FileMode.Create)
        use stream = new IO.StreamWriter(file, Text.Encoding.UTF8)
        saveTo stream.WriteLine sln
