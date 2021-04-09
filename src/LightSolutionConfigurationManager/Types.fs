namespace LightSolutionConfigurationManager

open System
open SolutionParser.Construction
open Microsoft.FSharp.Core.Printf

[<Struct>]
type GuidString = GuidString of string

[<RequireQualifiedAccess>]
module GuidString =
    let value (GuidString s) = s

type GuidString with
    member g.Value = GuidString.value g


type SolutionConfiguration =
    { Configuration: string
      Platform: string }

[<RequireQualifiedAccess>]
module SolutionConfiguration =
    let fullName ({ Configuration = c; Platform = p } as cfg) = // cfg for readable name in generated code, don't remove
        if String.IsNullOrEmpty p then
            c
        else
            sprintf "%s|%s" c (p.Replace("AnyCPU", "Any CPU"))

    let fromMSBuild (cfg: SolutionConfigurationInSolution) =
        { Configuration = cfg.ConfigurationName
          Platform = cfg.PlatformName }

type SolutionConfiguration with
    member c.FullName = SolutionConfiguration.fullName c


type ProjectConfiguration =
    { Configuration: SolutionConfiguration
      IncludeInBuild: bool }

[<RequireQualifiedAccess>]
module ProjectConfiguration =
    let fullName c =
        SolutionConfiguration.fullName c.Configuration

    let fromMSBuild (cfg: ProjectConfigurationInSolution) =
        { IncludeInBuild = cfg.IncludeInBuild
          Configuration =
              { Configuration = cfg.ConfigurationName
                Platform = cfg.PlatformName } }

type ProjectConfiguration with
    member c.FullName = ProjectConfiguration.fullName c


type Project =
    { ProjectGuid: GuidString
      ProjectTypeGuid: GuidString
      ParentProjectGuid: GuidString
      Name: string
      RelativePath: string
      FolderFiles: string list
      Dependencies: GuidString list
      Configurations: Map<string, ProjectConfiguration> }

[<RequireQualifiedAccess>]
module Project =
    let fromMSBuild (proj: ProjectInSolution) =
        { ProjectGuid = GuidString proj.ProjectGuid
          ProjectTypeGuid = GuidString proj.ProjectTypeGuid
          ParentProjectGuid = GuidString proj.ParentProjectGuid
          Name = proj.ProjectName
          RelativePath = proj.RelativePath
          FolderFiles = List.ofSeq proj.FolderFiles

          Dependencies =
              proj.Dependencies
              |> Seq.map GuidString
              |> Seq.toList

          Configurations =
              proj.ProjectConfigurations
              |> Seq.map (fun (KeyValue (k, v)) -> (k, ProjectConfiguration.fromMSBuild v))
              |> Map.ofSeq }


type Solution =
    { FormatVersion: int
      VisualStudioVersion: Version
      MinimumVisualStudioVersion: Version
      Configurations: SolutionConfiguration list
      ProjectsInOrder: Project list
      NestedProjectsInOrder: GuidString list
      SolutionGuid: GuidString }

[<RequireQualifiedAccess>]
module Solution =
    let projectsByGuid sln =
        sln.ProjectsInOrder
        |> List.fold (fun state p -> Map.add p.ProjectGuid p state) Map.empty

    let fromMSBuild (sln: SolutionFile) : Solution =
        { SolutionGuid = GuidString sln.SolutionGuid
          FormatVersion = sln.Version
          VisualStudioVersion = sln.VisualStudioVersionExact
          MinimumVisualStudioVersion = sln.MinimumVisualStudioVersionExact

          Configurations =
              sln.SolutionConfigurations
              |> Seq.map SolutionConfiguration.fromMSBuild
              |> Seq.toList

          ProjectsInOrder =
              sln.ProjectsInOrder
              |> Seq.map Project.fromMSBuild
              |> Seq.toList

          NestedProjectsInOrder =
              sln.NestedProjectsInOrder
              |> Seq.map GuidString
              |> Seq.toList }

    let private saveHeader writeLine sln =
        let v = $"Microsoft Visual Studio Solution File, Format Version %d{sln.FormatVersion}.00"
        let vsVm = $"# Visual Studio Version %d{sln.VisualStudioVersion.Major}"
        let vsV = $"VisualStudioVersion = %O{sln.VisualStudioVersion}"
        let mV = $"MinimumVisualStudioVersion = %O{sln.MinimumVisualStudioVersion}"
        writeLine v
        writeLine vsVm
        writeLine vsV
        writeLine mV

    let private saveProject writeLine project =
        writeLine $"Project(\"%s{project.ProjectTypeGuid.Value}\") = \"%s{project.Name}\", \"%s{project.RelativePath}\", \"%s{project.ProjectGuid.Value}\""

        if not project.FolderFiles.IsEmpty then
            writeLine "\tProjectSection(SolutionItems) = preProject"
            for file in project.FolderFiles do
                writeLine $"\t\t%s{file} = %s{file}"
            writeLine "\tEndProjectSection"

        if not project.Dependencies.IsEmpty then
            writeLine "\tProjectSection(ProjectDependencies) = postProject"
            for dep in project.Dependencies do
                writeLine $"\t\t%s{dep.Value} = %s{dep.Value}"
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
                let sc = slnCfg.FullName
                match proj.Configurations.TryGetValue sc with
                | (true, cfg) ->
                    writeLine $"\t\t%s{proj.ProjectGuid.Value}.%s{sc}.ActiveCfg = %s{cfg.FullName}"
                    if cfg.IncludeInBuild then
                        writeLine $"\t\t%s{proj.ProjectGuid.Value}.%s{sc}.Build.0 = %s{cfg.FullName}"
                | (false, _)  -> ()

        writeLine "\tEndGlobalSection"

    let private saveSolutionProperties writeLine =
        writeLine "\tGlobalSection(SolutionProperties) = preSolution"
        writeLine "\t\tHideSolutionNode = FALSE"
        writeLine "\tEndGlobalSection"

    let private saveNestedProjects writeLine projectsByGuid projects =
        writeLine "\tGlobalSection(NestedProjects) = preSolution"

        for guid in projects do
            let proj = Map.find guid projectsByGuid
            writeLine $"\t\t%s{proj.ProjectGuid.Value} = %s{proj.ParentProjectGuid.Value}"

        writeLine "\tEndGlobalSection"

    let private saveExtensibility writeLine sln =
        writeLine "\tGlobalSection(ExtensibilityGlobals) = postSolution"
        writeLine $"\t\tSolutionGuid = %s{sln.SolutionGuid.Value}"
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
        let sb = System.Text.StringBuilder()
        saveTo (sb.AppendLine >> ignore) sln
        sb

    let saveToFile path sln =
        use file = IO.File.OpenWrite path
        use stream = new IO.StreamWriter(file, Text.Encoding.UTF8)
        saveTo stream.WriteLine sln
