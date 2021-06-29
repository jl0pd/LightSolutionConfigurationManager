module LightSolutionManager.MainView

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open FSharp.Control.Tasks
open Elmish

type ViewState =
    | NoState
    | ProjCfgsState of ProjectConfigurationsView.State
    | ProjTreeState of SolutionTreeView.State

type LoadedState =
    { State : ViewState
      Solution : Solution }

type State =
    | NotLoaded
    | Loaded of LoadedState

type ViewMsg =
    | ProjCfgsMsg of ProjectConfigurationsView.Msg

type Views =
    | EmptyView
    | ProjCfgsView
    | ProjTreeView

type Msg =
    | ShowView of Views
    | SelectFile
    | FileSelected of string
    | FileNotSelected
    | SaveFile
    | FileSaved
    | ViewMsg of ViewMsg

let init () = NotLoaded, Cmd.none

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

        let! files = dlg.ShowAsync(getMainWindow())
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
    | SelectFile -> state, selectFileAsync()
    | SaveFile ->
        match state with
        | Loaded { Solution = sln } -> state, saveFileAsync sln
        | NotLoaded -> state, Cmd.none

    | FileSelected p ->
        Loaded { State = NoState; Solution = SolutionParser.parseFile p }, Cmd.none

    | ShowView EmptyView ->
        match state with
        | Loaded lo ->
            Loaded { lo with State = NoState }, Cmd.none
        | NotLoaded -> state, Cmd.none

    | ShowView ProjTreeView ->
        match state with
        | Loaded lo ->
            Loaded { lo with State = ProjTreeState (SolutionTreeView.init lo.Solution) }, Cmd.none
        | NotLoaded -> state, Cmd.none

    | ShowView ProjCfgsView ->
        match state with
        | Loaded ({ Solution = sln } as lo) ->
            let s, cmd = ProjectConfigurationsView.init sln
            Loaded { lo with State = ProjCfgsState s }, cmd
        | NotLoaded -> state, Cmd.none

    | ViewMsg (ProjCfgsMsg m) ->
        match state with
        | Loaded ({ State = ProjCfgsState p } as lo) ->
            let s, cmd = ProjectConfigurationsView.update p m
            Loaded { lo with State = ProjCfgsState s }, cmd
        | _ -> state, Cmd.none

    | FileNotSelected
    | FileSaved -> state, Cmd.none

let selectSolutionView dispatch =
    Border.create [
        Border.child
            (Button.create [
                Button.verticalAlignment VerticalAlignment.Center
                Button.horizontalAlignment HorizontalAlignment.Center
                Button.content "Load file"
                Button.onClick (fun _ -> dispatch SelectFile)
            ])
    ]

let loadedView subView additionalControls showBackButton dispatch =
    let topPanel =
        StackPanel.create [
            StackPanel.dock Dock.Top
            StackPanel.orientation Orientation.Horizontal
            StackPanel.background "#222255"
            StackPanel.children [
                if showBackButton then
                    Button.create [
                        Button.content "<-"
                        Button.onClick (fun _ -> dispatch (ShowView EmptyView))
                    ]

                Button.create [
                    Button.content "Load file"
                    Button.onClick (fun _ -> dispatch SelectFile)
                ]
                Button.create [
                    Button.content "Save file"
                    Button.onClick (fun _ -> dispatch SaveFile)
                ]

                yield! additionalControls
            ]
        ]

    DockPanel.create [
        DockPanel.children [
            topPanel
            subView
        ]
    ]

let emptyView dispatch =
    StackPanel.create [
        StackPanel.verticalAlignment VerticalAlignment.Center
        StackPanel.horizontalAlignment HorizontalAlignment.Center
        StackPanel.children [
            Button.create [
                Button.content "Edit project configurations"
                Button.onClick (fun _ -> dispatch (ShowView ProjCfgsView))
            ]
            Button.create [
                Button.content "View project tree"
                Button.onClick (fun _ -> dispatch (ShowView ProjTreeView))
            ]
        ]
    ]

let view state dispatch =
    match state with
    | NotLoaded ->
        selectSolutionView dispatch :> IView

    | Loaded { State = NoState } ->
        loadedView (emptyView dispatch) [] false dispatch :> IView

    | Loaded { State = ProjTreeState x } ->
        loadedView (SolutionTreeView.view x ignore) [] true dispatch :> IView

    | Loaded { State = ProjCfgsState x } ->
        let (view, additional) = ProjectConfigurationsView.viewPair x (ProjCfgsMsg >> ViewMsg >> dispatch)
        loadedView view additional true dispatch :> IView
