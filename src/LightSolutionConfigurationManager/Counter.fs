module LightSolutionConfigurationManager.Counter

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.FuncUI.Components
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open FSharp.Control.Tasks
open Elmish
open SolutionParser.Construction


type LoadedSolution =
    { Solution : Solution
      SolutionFile : SolutionFile
      SelectedConfiguration : SolutionConfiguration }
    with
        static member Create file =
            let sln = Solution.fromMSBuild file
            { SolutionFile = file
              Solution = sln
              SelectedConfiguration = List.head sln.Configurations }

type State =
    | SolutionNotSelected
    | SolutionIsLoaded of LoadedSolution

type Msg =
    | SelectFile
    | FileSelected of string
    | ConfigurationSelected of SolutionConfiguration
    | FileNotSelected
    | SaveFile
    | FileSaved

let init () = SolutionNotSelected, Cmd.none

let getMainWindow () = // TODO: move somewhere
    (Application.Current.ApplicationLifetime :?> IClassicDesktopStyleApplicationLifetime).MainWindow

let selectFileAsync () =
    task {
        let dlg = OpenFileDialog(
                    AllowMultiple = false,
                    Filters = ResizeArray [
                                FileDialogFilter(
                                    Name = "Solution file",
                                    Extensions = ResizeArray [ "sln" ])
                    ])

        let! files = dlg.ShowAsync (getMainWindow())
        if files.Length > 0 then
            return FileSelected files.[0]
        else
            return FileNotSelected
    } |> Cmd.OfTask.result

let saveFileAsync sln =
    task {
        let dlg = SaveFileDialog(DefaultExtension = "sln")
        let! path = dlg.ShowAsync (getMainWindow())
        Solution.saveToFile path sln
        return FileSaved
    } |> Cmd.OfTask.result

let update (msg: Msg) (state: State) : (State * Cmd<Msg>) =
    match msg with
    | SelectFile ->
        state, selectFileAsync()
    | SaveFile ->
        match state with
        | SolutionIsLoaded sln -> state, saveFileAsync sln.Solution
        | SolutionNotSelected -> state, Cmd.none

    | FileSelected p ->
        SolutionIsLoaded (SolutionFile.Parse p |> LoadedSolution.Create), Cmd.none

    | ConfigurationSelected c ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln -> SolutionIsLoaded { sln with SelectedConfiguration = c }, Cmd.none

    | FileNotSelected
    | FileSaved -> state, Cmd.none

let isFolder project =
    project.ProjectTypeGuid.Value = "{2150E333-8FDC-42A3-9474-1A3956D46DE8}"

let projectView (project: Project) (selectedCfg: SolutionConfiguration) index dispatch =
    let allBuild =
        let i, ni = project.Configurations
                    |> Seq.partition (fun (KeyValue (_, c)) -> c.IncludeInBuild)

        let ic, nic = Seq.length i, Seq.length ni

        if ic = 0 then
            Nullable false
        elif nic <> 0 then
            Nullable ()
        else
            Nullable true

    DockPanel.create [
        DockPanel.children [
            TextBlock.create [
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.text project.Name
                TextBlock.minWidth 250.
            ]

            UniformGrid.create [
                UniformGrid.rows 2
                UniformGrid.columns 2

                let projCfg = project.Configurations.[selectedCfg.FullName]

                UniformGrid.children [
                    TextBlock.create [
                        TextBlock.text
                            (sprintf "Configuration: %s" projCfg.Configuration.Configuration)
                    ]

                    TextBlock.create [
                        TextBlock.text
                            (sprintf "Platform: %s" projCfg.Configuration.Platform)
                    ]

                    CheckBox.create [
                        CheckBox.margin 5.
                        CheckBox.content "build in all configurations"
                        CheckBox.isChecked allBuild
                    ]

                    CheckBox.create [
                        CheckBox.margin 5.
                        CheckBox.content "build"
                        CheckBox.isChecked projCfg.IncludeInBuild
                    ]
                ]
            ]
        ]
    ]

let emptyView dispatch =
    Border.create [
        Border.child
            (Button.create [
                Button.verticalAlignment VerticalAlignment.Center
                Button.horizontalAlignment HorizontalAlignment.Center
                Button.content "Load file"
                Button.onClick (fun _ -> dispatch SelectFile)
            ])
    ]

let loadedView (state: LoadedSolution) dispatch =
    let topPanel =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                Button.create [
                    Button.content "Load file"
                    Button.onClick (fun _ -> dispatch SelectFile)
                ]
                Button.create [
                    Button.content "Save file"
                    Button.onClick (fun _ -> dispatch SaveFile)
                ]

                ComboBox.create [
                    ComboBox.margin (10., 1.)
                    ComboBox.minWidth 200.
                    ComboBox.horizontalAlignment HorizontalAlignment.Right

                    ComboBox.dataItems state.Solution.Configurations
                    ComboBox.itemTemplate
                            (DataTemplateView<SolutionConfiguration>.create
                                (fun c ->TextBlock.create [TextBlock.text c.FullName ]))

                    ComboBox.selectedItem state.SelectedConfiguration
                    ComboBox.onSelectedItemChanged
                        (fun o ->
                            match o with
                            | :? SolutionConfiguration as c -> dispatch <| ConfigurationSelected c
                            | _ -> ())
                ]
            ]
        ]

    let projects =
        ListBox.create [
            ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Disabled

            ListBox.dataItems
                (state.Solution.ProjectsInOrder
                |> List.where (not << isFolder)
                |> List.indexed
                |> List.map (fun p -> (p, state.SelectedConfiguration)))

            ListBox.itemTemplate
                (DataTemplateView<((int * Project) * SolutionConfiguration)>.create
                    (fun ((i, p), c) -> projectView p c i dispatch))
        ]

    DockPanel.create [
        DockPanel.children [
            topPanel
            projects
        ]
    ]

let view state dispatch =
    match state with
    | SolutionIsLoaded sln -> loadedView sln dispatch :> IView
    | SolutionNotSelected -> emptyView dispatch :> IView
