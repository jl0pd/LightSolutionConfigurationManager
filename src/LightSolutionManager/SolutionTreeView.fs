module LightSolutionManager.SolutionTreeView


open System.IO
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Components
open LightSolutionManager.SolutionStructure
open Avalonia.Controls.Primitives
open LightSolutionManager.Extensions

type State =
    { Solution : Solution
      Tree : SolutionTree }

type Msg =
    | ProjectSelected of Project

let init sln =
    { Solution = sln
      Tree = TreeBuilder.buildSolutionTree sln }

let view state dispatch =
    DockPanel.create [
        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Top
                TextBlock.text
                    (state.Solution.FileName
                    |> Option.map Path.GetFileNameWithoutExtension
                    |> Option.defaultValue "Solution file")
            ]

            TreeView.create [
                TreeView.verticalScrollBarVisibility ScrollBarVisibility.Visible
                TreeView.dataItems state.Tree.Projects
                TreeView.itemTemplate
                    (DataTemplateView<ProjectNode>.create(
                        (fun n -> n.Children :> _ seq),
                        (fun { Project = proj } ->
                            TextBlock.create [
                                let label = if proj.IsFolder then
                                                proj.Name
                                            else
                                                let ext =
                                                    proj.Path.OriginalString
                                                    |> Path.GetExtension
                                                    |> String.skip 1
                                                let kind =
                                                    if ext |> String.Ordinal.endsWith "proj" then
                                                        ext |> String.skipLast 4
                                                    else
                                                        ext
                                                "[" + kind + "] " + proj.Name // TODO: use pictures

                                TextBlock.text label
                                TextBlock.onTapped ((fun _ -> dispatch (ProjectSelected proj)), OnChangeOf proj)
                            ])
                    ))
            ]
        ]
    ]
