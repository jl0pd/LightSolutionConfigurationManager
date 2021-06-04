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
    | ProjectConfigurationSelected of int * SolutionConfiguration
    | ChangeIncludeInBuild of int * bool
    | ChangeAllIncludeInBuild of int * bool
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

let patchProject map index (projects: Project list) =
    let rec inner i acc = function
        | [] -> acc
        | x :: xs when Project.isFolder x -> inner i (x :: acc) xs
        | x :: xs when i = 0 -> inner (i - 1) ((map x) :: acc) xs
        | x :: xs -> inner (i - 1) (x :: acc) xs
    inner index [] projects |> List.rev

let patchProjectConfigurations map index projects =
    patchProject
        (fun p ->
            let newCfgs =
                p.Configurations
                |> Seq.map map
                |> Map.ofSeq
            { p with Configurations = newCfgs }
        )
        index
        projects

let changeIncludeInBuild' index isIncluded shouldChange projects =
    patchProjectConfigurations
        (fun (KeyValue (k, c)) ->
            if shouldChange k
            then (k, { c with IncludeInBuild = isIncluded })
            else (k, c))
        index
        projects

let changeIncludeInBuild index isIncluded (solutionCfg: SolutionConfiguration) projects =
    changeIncludeInBuild' index isIncluded (fun s -> s = solutionCfg.FullName) projects

let changeAllIncludeInBuild index isIncluded projects =
    changeIncludeInBuild' index isIncluded (fun _ -> true) projects

let changeProjectBuildConfiguration index (solutionCfg: SolutionConfiguration) (newCfg: SolutionConfiguration) projects =
    patchProjectConfigurations
        (fun (KeyValue (k, v)) ->
            if k = solutionCfg.FullName then
                (k, { v with Configuration = newCfg })
            else (k, v))
        index
        projects

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

    | ChangeIncludeInBuild (index, isIncluded) ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            let patched = changeIncludeInBuild index isIncluded sln.SelectedConfiguration sln.Solution.ProjectsInOrder
            SolutionIsLoaded { sln with Solution = { sln.Solution with ProjectsInOrder = patched } }, Cmd.none

    | ChangeAllIncludeInBuild (index, isIncluded) ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            let patched = changeAllIncludeInBuild index isIncluded sln.Solution.ProjectsInOrder
            SolutionIsLoaded { sln with Solution = { sln.Solution with ProjectsInOrder = patched } }, Cmd.none

    | ProjectConfigurationSelected (index, cfg) ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            let patched = changeProjectBuildConfiguration index sln.SelectedConfiguration cfg sln.Solution.ProjectsInOrder
            SolutionIsLoaded { sln with Solution = { sln.Solution with ProjectsInOrder = patched } }, Cmd.none

    | FileNotSelected
    | FileSaved -> state, Cmd.none

let projectView (project: Project) (selectedCfg: SolutionConfiguration) (solutionCfgs: SolutionConfiguration list) (index: int) (dispatch: Msg -> unit) =
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

    let projCfg = project.Configurations.[selectedCfg.FullName]

    DockPanel.create [
        DockPanel.children [
            TextBlock.create [
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.text project.Name
                TextBlock.minWidth 250.
            ]

            Grid.create [
                Grid.columnDefinitions "auto,100"
                Grid.rowDefinitions "*,*"

                Grid.children [
                    StackPanel.create [
                        StackPanel.columnSpan 2
                        StackPanel.children [
                            ComboBox.create [
                                ComboBox.minWidth 200.
                                ComboBox.itemTemplate
                                    (DataTemplateView<SolutionConfiguration>.create
                                        (fun c -> TextBlock.create [ TextBlock.text c.FullName ]))
                                ComboBox.dataItems solutionCfgs
                                ComboBox.selectedItem projCfg.Configuration
                                ComboBox.onSelectedItemChanged
                                    ((function
                                      | :? SolutionConfiguration as c ->
                                            dispatch (ProjectConfigurationSelected (index, c))
                                      | _ -> ()), OnChangeOf index)
                            ]
                        ]
                    ]

                    CheckBox.create [
                        CheckBox.margin 5.
                        CheckBox.row 1
                        CheckBox.content "build in all configurations"
                        CheckBox.isChecked allBuild
                        CheckBox.onUnchecked ((fun _ -> dispatch (ChangeAllIncludeInBuild (index, false))), OnChangeOf index)
                        CheckBox.onChecked ((fun _ -> dispatch (ChangeAllIncludeInBuild (index, true))), OnChangeOf index)
                    ]

                    CheckBox.create [
                        CheckBox.row 1
                        CheckBox.column 1
                        CheckBox.margin 5.
                        CheckBox.content "build"
                        CheckBox.isChecked projCfg.IncludeInBuild
                        CheckBox.onUnchecked ((fun _ -> dispatch (ChangeIncludeInBuild (index, false))), OnChangeOf index)
                        CheckBox.onChecked ((fun _ -> dispatch (ChangeIncludeInBuild (index, true))), OnChangeOf index)
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
                                (fun c -> TextBlock.create [TextBlock.text c.FullName ]))

                    ComboBox.selectedItem state.SelectedConfiguration
                    ComboBox.onSelectedItemChanged
                        (function
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
                |> List.vChoosei (fun i p ->
                    if p.IsFolder then
                        ValueNone
                    else
                        ValueSome (i, p, state.SelectedConfiguration, state.Solution.Configurations)))

            ListBox.itemTemplate
                (DataTemplateView<(int * Project * SolutionConfiguration * SolutionConfiguration list)>.create
                    (fun (i, p, c, cs) -> projectView p c cs i dispatch))
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
