module LightSolutionManager.ProjectConfigurationsView

open System
open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.FuncUI.Components
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.Types
open LightSolutionManager.Extensions

type SearchOptions =
    { Pattern : string
      IsRegex : bool
      IsCaseSensitive : bool }

type State =
    { Solution : Solution
      SelectedConfiguration : SolutionConfiguration
      SearchOptions : SearchOptions }

type Msg =
    | RegexToggled of bool
    | CaseSensitiveToggled of bool
    | SearchTextChanged of string
    | ConfigurationSelected of SolutionConfiguration
    | ProjectConfigurationSelected of Guid * SolutionConfiguration
    | ChangeIncludeInBuild of Guid * bool
    | ChangeAllIncludeInBuild of Guid * bool

let init sln =
    { Solution = sln
      SelectedConfiguration = List.head sln.Configurations
      SearchOptions = { Pattern = ""
                        IsRegex = false
                        IsCaseSensitive = false } }, Cmd.none

let update state msg =
    match msg with
    | ConfigurationSelected c ->
        { state with SelectedConfiguration = c }, Cmd.none

    | ChangeIncludeInBuild (id, isIncluded) ->
        { state with
                    Solution = Solution.patchProject
                                (fun p -> { p with Configurations =
                                                    Map.change
                                                        state.SelectedConfiguration
                                                        (function
                                                        | None -> None
                                                        | Some c -> Some { c with IncludeInBuild = isIncluded })
                                                        p.Configurations })
                                id
                                state.Solution }
        , Cmd.none

    | ChangeAllIncludeInBuild (id, isIncluded) ->
        { state with
                Solution = Solution.patchProject
                            (fun p -> { p with Configurations =
                                                Map.map
                                                    (fun _ value -> { value with IncludeInBuild = isIncluded })
                                                    p.Configurations })
                            id
                            state.Solution }
        , Cmd.none

    | ProjectConfigurationSelected (id, cfg) ->
        { state with
                Solution = Solution.patchProject
                            (fun p -> { p with Configurations =
                                                Map.change
                                                    state.SelectedConfiguration
                                                    (function
                                                    | None -> None
                                                    | Some c -> Some { c with Configuration = cfg })
                                                    p.Configurations })
                            id
                            state.Solution }
        , Cmd.none

    | SearchTextChanged s ->
        { state with SearchOptions = { state.SearchOptions with Pattern = s } }, Cmd.none

    | RegexToggled r ->
        { state with SearchOptions = { state.SearchOptions with IsRegex = r } }, Cmd.none

    | CaseSensitiveToggled c ->
        { state with SearchOptions = { state.SearchOptions with IsCaseSensitive = c } }, Cmd.none


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


let additionalControls state dispatch : IView list =
    [
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


let view state dispatch =
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


let viewPair state dispatch =
    (view state dispatch, additionalControls state dispatch)
