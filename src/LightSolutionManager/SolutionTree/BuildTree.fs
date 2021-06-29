namespace LightSolutionManager.SolutionStructure

open System
open System.Collections.Generic
open LightSolutionManager

type ProjectNode =
    { Project : Project
      Children : ProjectNode list }

type SolutionTree =
    { Solution : Solution
      Projects : ProjectNode list }

module TreeBuilder =

    type private Folder(project: Project) =
        member val Id = project.Id
        member val Project = project
        member val SubProjects = ResizeArray<Project>()
        member val SubFolders = ResizeArray<Folder>()
        member self.ToProjectNode() =
                let projectChildren = self.SubProjects
                                      |> Seq.map (fun p -> { Project = p; Children = [] })

                let folderChildren = self.SubFolders
                                     |> Seq.map (fun f -> f.ToProjectNode())

                { Project = self.Project; Children = Seq.concat [folderChildren; projectChildren] |> Seq.toList }

    type private FolderOrProject =
        | FolderF of Folder
        | ProjectP of Project

    type private MutableTree(sln: Solution) =
        member val Solution = sln
        member val Rooted = ResizeArray<FolderOrProject>()
        member val Folders = Dictionary<Guid, Folder>() with get, set

    let private getProjectGroups (sln: Solution) =
        let rooted = ResizeArray() // can contain projects & folders
        let folders = Dictionary() // only folders
        let notRootedProjects = ResizeArray() // not rooted projects

        for proj in sln.ProjectsInOrder.Values do
            if not proj.IsFolder && proj.ParentId.IsSome then
                notRootedProjects.Add proj
            else
                if proj.ParentId.IsNone then
                    rooted.Add proj

                if proj.IsFolder then
                    folders.[proj.Id] <- proj

        {| Rooted = rooted; Folders = folders; NotRootedProjects = notRootedProjects |}

    let buildSolutionTree (sln: Solution) : SolutionTree =
        let mutTree = MutableTree sln

        let groups = getProjectGroups sln

        for KeyValue (id, folder) in groups.Folders do
            mutTree.Folders.[id] <- Folder folder
        
        for rooted in groups.Rooted do
            mutTree.Rooted.Add (
                if rooted.IsFolder then
                    FolderF mutTree.Folders.[rooted.Id]
                else
                    ProjectP rooted
            )

        for folder in mutTree.Folders.Values do
            match folder.Project.ParentId with
            | Some id ->
                mutTree.Folders.[id].SubFolders.Add folder
            | None -> () // rooted folder

        for proj in groups.NotRootedProjects do
            match proj.ParentId with
            | None -> invalidOp "Project doesn't have parent id" // never get here
            | Some id ->
                mutTree.Folders.[id].SubProjects.Add proj

        let projects = mutTree.Rooted
                       |> Seq.sortWith (fun left right ->
                                            match left, right with
                                            | FolderF le, FolderF ri -> le.Project.Name.CompareTo ri.Project.Name
                                            | ProjectP le, ProjectP ri -> le.Name.CompareTo ri.Name
                                            | FolderF _, ProjectP _ -> -1
                                            | ProjectP _, FolderF _ -> 1)
                       |> Seq.map (function
                                   | FolderF folder -> folder.ToProjectNode()
                                   | ProjectP proj -> { Project = proj; Children = [] })
                       |> Seq.toList

        { Solution = sln
          Projects = projects }
