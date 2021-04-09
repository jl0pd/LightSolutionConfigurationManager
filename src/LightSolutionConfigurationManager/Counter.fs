module LightSolutionConfigurationManager.Counter

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open FSharp.Control.Tasks
open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open SolutionParser.Construction

type State =
    | SolutionNotSelected
    | SolutionIsLoaded of Solution

type Msg =
    | SelectFile
    | FileSelected of string
    | FileNotSelected
    | SaveFile
    | FileSaved

let init () = SolutionNotSelected, Cmd.none

let getMainWindow () = // TODO: move somewhere
    (Application.Current.ApplicationLifetime :?> IClassicDesktopStyleApplicationLifetime).MainWindow

let selectFileAsync () =
    task { 
        let dlg =  OpenFileDialog(AllowMultiple = false, Filters = ResizeArray [ FileDialogFilter(Name = "Solution file", Extensions = ResizeArray [ "sln" ] ) ])
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
        | SolutionIsLoaded sln -> state, saveFileAsync sln
        | SolutionNotSelected -> state, Cmd.none
    
    | FileSelected p -> 
        SolutionIsLoaded (SolutionFile.Parse p |> Solution.fromMSBuild), Cmd.none
    
    | FileNotSelected 
    | FileSaved -> state, Cmd.none

let view (state: State) (dispatch) =
    StackPanel.create [
        StackPanel.children [
            Button.create [
                Button.content "Pick file"
                Button.onClick (fun _ -> dispatch SelectFile)
            ]
            Button.create [
                Button.content "Save file"
                Button.onClick (fun _ -> dispatch SaveFile)
            ]
        ]
    ]
