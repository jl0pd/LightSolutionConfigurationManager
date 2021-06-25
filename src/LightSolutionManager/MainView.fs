module LightSolutionManager.MainView

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

type SearchOptions =
    { Pattern : string
      IsRegex : bool
      IsCaseSensitive : bool }

type LoadedSolution =
    { Solution : Solution
      SelectedConfiguration : SolutionConfiguration
      SearchOptions : SearchOptions }
    with
        static member FromFile path =
            let sln = SolutionParser.parseFile path
            { Solution = sln
              SelectedConfiguration = List.head sln.Configurations
              SearchOptions = { Pattern = ""; IsRegex = false; IsCaseSensitive = false } }

type State =
    | SolutionNotSelected
    | SolutionIsLoaded of LoadedSolution

type Msg =
    | SelectFile
    | FileSelected of string
    | RegexToggled of bool
    | CaseSensitiveToggled of bool
    | SearchTextChanged of string
    | ConfigurationSelected of SolutionConfiguration
    | ProjectConfigurationSelected of Guid * SolutionConfiguration
    | ChangeIncludeInBuild of Guid * bool
    | ChangeAllIncludeInBuild of Guid * bool
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
        if not <| String.IsNullOrWhiteSpace path then
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
        SolutionIsLoaded (LoadedSolution.FromFile p), Cmd.none

    | ConfigurationSelected c ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln -> SolutionIsLoaded { sln with SelectedConfiguration = c }, Cmd.none

    | ChangeIncludeInBuild (id, isIncluded) ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            SolutionIsLoaded { sln with
                                    Solution = Solution.patchProject
                                                (fun p -> { p with Configurations =
                                                                    Map.change
                                                                        sln.SelectedConfiguration
                                                                        (function
                                                                        | None -> None
                                                                        | Some c -> Some { c with IncludeInBuild = isIncluded })
                                                                        p.Configurations })
                                                id
                                                sln.Solution }
            , Cmd.none

    | ChangeAllIncludeInBuild (id, isIncluded) ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            SolutionIsLoaded { sln with
                                    Solution = Solution.patchProject
                                                (fun p -> { p with Configurations =
                                                                    Map.map
                                                                        (fun _ value -> { value with IncludeInBuild = isIncluded })
                                                                        p.Configurations })
                                                id
                                                sln.Solution }
            , Cmd.none

    | ProjectConfigurationSelected (id, cfg) ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            SolutionIsLoaded { sln with
                                    Solution = Solution.patchProject
                                                (fun p -> { p with Configurations =
                                                                    Map.change
                                                                        sln.SelectedConfiguration
                                                                        (function
                                                                        | None -> None
                                                                        | Some c -> Some { c with Configuration = cfg })
                                                                        p.Configurations })
                                                id
                                                sln.Solution }
            , Cmd.none

    | SearchTextChanged s ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            SolutionIsLoaded { sln with SearchOptions = { sln.SearchOptions with Pattern = s } }, Cmd.none

    | RegexToggled r ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            SolutionIsLoaded { sln with SearchOptions = { sln.SearchOptions with IsRegex = r } }, Cmd.none

    | CaseSensitiveToggled c ->
        match state with
        | SolutionNotSelected -> state, Cmd.none
        | SolutionIsLoaded sln ->
            SolutionIsLoaded { sln with SearchOptions = { sln.SearchOptions with IsCaseSensitive = c } }, Cmd.none

    | FileNotSelected
    | FileSaved -> state, Cmd.none

let projectView (project: Project) (selectedCfg: SolutionConfiguration) (solutionCfgs: SolutionConfiguration list) (dispatch: Msg -> unit) =
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

    let projCfg = project.Configurations.[selectedCfg]

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
                                            dispatch (ProjectConfigurationSelected (project.Id, c))
                                      | _ -> ()), OnChangeOf project.Id)
                            ]
                        ]
                    ]

                    CheckBox.create [
                        CheckBox.margin 5.
                        CheckBox.row 1
                        CheckBox.content "build in all configurations"
                        CheckBox.isChecked allBuild
                        CheckBox.onUnchecked ((fun _ -> dispatch (ChangeAllIncludeInBuild (project.Id, false))), OnChangeOf project.Id)
                        CheckBox.onChecked ((fun _ -> dispatch (ChangeAllIncludeInBuild (project.Id, true))), OnChangeOf project.Id)
                    ]

                    CheckBox.create [
                        CheckBox.row 1
                        CheckBox.column 1
                        CheckBox.margin 5.
                        CheckBox.content "build"
                        CheckBox.isChecked projCfg.IncludeInBuild
                        CheckBox.onUnchecked ((fun _ -> dispatch (ChangeIncludeInBuild (project.Id, false))), OnChangeOf project.Id)
                        CheckBox.onChecked ((fun _ -> dispatch (ChangeIncludeInBuild (project.Id, true))), OnChangeOf project.Id)
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

                TextBox.create [
                    TextBox.text state.SearchOptions.Pattern
                    TextBox.minWidth 100.
                    TextBox.onTextChanged (dispatch << SearchTextChanged)
                ]

                ToggleButton.create [
                    ToggleButton.isChecked state.SearchOptions.IsRegex
                    ToggleButton.content "*."
                    ToggleButton.padding 0.
                    ToggleButton.width 30.
                    ToggleButton.height 30.
                    ToggleButton.onChecked (fun _ -> dispatch (RegexToggled true))
                    ToggleButton.onUnchecked (fun _ -> dispatch (RegexToggled false))
                ]

                ToggleButton.create [
                    ToggleButton.isChecked state.SearchOptions.IsCaseSensitive
                    ToggleButton.content "Aa"
                    ToggleButton.padding 0.
                    ToggleButton.width 30.
                    ToggleButton.height 30.
                    ToggleButton.onChecked (fun _ -> dispatch (CaseSensitiveToggled true))
                    ToggleButton.onUnchecked (fun _ -> dispatch (CaseSensitiveToggled false))
                ]
            ]
        ]

    let projects =
        ListBox.create [
            ListBox.horizontalScrollBarVisibility ScrollBarVisibility.Disabled

            ListBox.dataItems
                (state.Solution.ProjectsInOrder
                |> Seq.choose (fun (KeyValue(_, p)) ->
                    if p.IsFolder then
                        None
                    else
                        let { Pattern = pattern
                              IsRegex = isRegex
                              IsCaseSensitive = isCaseSensitive } = state.SearchOptions
                        if (String.IsNullOrEmpty pattern
                        ||     isRegex &&     isCaseSensitive && Regex.isMatch p.Name pattern
                        ||     isRegex && not isCaseSensitive && Regex.isMatchCaseInsensitive p.Name pattern
                        || not isRegex &&     isCaseSensitive && p.Name.Contains(pattern, StringComparison.InvariantCulture)
                        || not isRegex && not isCaseSensitive && p.Name.Contains(pattern, StringComparison.InvariantCultureIgnoreCase)) then
                            Some (p, state.SelectedConfiguration, state.Solution.Configurations)
                        else
                            None)
                |> Seq.toArray)

            ListBox.itemTemplate
                (DataTemplateView<(Project * SolutionConfiguration * SolutionConfiguration list)>.create
                    (fun (p, c, cs) -> projectView p c cs dispatch))
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
