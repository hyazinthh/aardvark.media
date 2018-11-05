﻿module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open Model
open Provenance
open Story

let initial =
    let model = BoxSelectionApp.initial in {
        appModel = model
        dockConfig = config {
                        content (
                            vertical 1.0 [
                                horizontal 5.0 [
                                    element { id "render"; title "Render View"; isCloseable false; weight 10 }

                                    stack 3.5 (Some "controls") [
                                        { id = "controls"; title = Some "Controls"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                                        { id = "presentation"; title = Some "Presentation"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                                    ]
                                ]

                                element { id "provenance"; title "History"; isCloseable true; weight 1 }
                                element { id "storyboard"; title "Storyboard"; isCloseable true; weight 1.25 }
                            ]
                        )
                        appName "Box Selection"
                        useCachedConfig false
                    }
        provenance = ProvenanceApp.init model
        story = StoryApp.init
        presentation = false
        animation = AnimationApp.init model
    }

let update (model : Model) (act : Action) = 
    match act with
        | AnimationAction a ->
            model |> AnimationApp.update a

        | ProvenanceAction a ->
            let p = model.provenance |> ProvenanceApp.update model.story a
            let m = ProvenanceApp.restore model.appModel p

            model |> Lens.set Model.Lens.provenance p
                  |> Lens.set Model.Lens.appModel m
        
        | StoryAction a ->
            model |> StoryApp.update a

        | AppAction a when not (Model.isAnimating model) ->
            let s = BoxSelectionApp.update model.appModel a
            let p = model.provenance |> ProvenanceApp.update model.story (Update (s, a))
            
            model |> Lens.set Model.Lens.appModel s
                  |> Lens.set Model.Lens.provenance p

        | KeyDown Keys.Z ->
            model |> Lens.update Model.Lens.provenance (ProvenanceApp.update model.story Undo)

        | KeyDown Keys.R ->
            { model with dockConfig = initial.dockConfig }

        | KeyDown Keys.P ->
            { model with presentation = true }

        | KeyDown Keys.Escape ->
            { model with presentation = false }

        | KeyDown Keys.Right 
        | KeyDown Keys.Enter ->
            model |> StoryApp.update Forward

        | KeyDown Keys.Left
        | KeyDown Keys.Back ->
            model |> StoryApp.update Backward

        | UpdateConfig cfg ->
            { model with dockConfig = cfg }

        | _ -> 
            model

let threads (model : Model) =
    
    // Thread pool for actual application
    let appThreads = model.appModel |> BoxSelectionApp.app.threads 
                                    |> ThreadPool.map AppAction

    // Thread pool for animations
    let animationThreads = model |> AnimationApp.threads

    // Thread pool for story module                                                 
    let storyThreads = model |> StoryApp.threads
                             |> ThreadPool.map StoryAction

    [appThreads; animationThreads; storyThreads]
        |> ThreadPool.unionMany


let renderView (model : MModel) =
    // TODO: Adding keyboard events here breaks input events
    // for the text areas of the annotations
    body [ (*onKeyDown KeyDown; onKeyUp KeyUp*) ] [
        model.appModel
            |> BoxSelectionApp.renderView
            |> UI.map AppAction

        model |> StoryApp.overlayView
              |> UI.map StoryAction
    ]

let controlsView (model : MModel) =
    body [style "background-color:#1B1C1E"] [
        model.appModel
            |> BoxSelectionApp.controlsView
            |> UI.map AppAction
    ]

let provenanceView (model : MModel) =
    body [] [
        model.provenance
            |> ProvenanceApp.view
            |> UI.map ProvenanceAction
    ]

let storyboardView (model : MModel) =
    body [] [
        model |> StoryApp.storyboardView
              |> UI.map StoryAction
    ]

let presentationView (model : MModel) =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "presentationStyle"; url = "Presentation.css" }
    ]

    require (dependencies) (
        body [clazz "ui"] [
            Html.SemUi.accordion "Rendering" "options" true [
                model.appModel |> BoxSelectionApp.renderingControlsView |> UI.map AppAction
            ]
        ]
    )

let view (model : MModel) =
    page (fun request ->
        match Map.tryFind "page" request.queryParams with
            | Some "render" -> 
                renderView model               
            | Some "controls" -> 
                controlsView model
            | Some "provenance" ->
                provenanceView model
            | Some "storyboard" ->
                storyboardView model
            | Some "presentation" ->
                presentationView model
            | Some other ->
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color:white; font-size:large; background-color:red; width:100%; height:100%"] [text msg]
                ]  
            | None ->
                model.dockConfig |> docking [
                    style "width:100%; height:100%; overflow:hidden"
                    onLayoutChanged UpdateConfig
                ]
    )
 
let app : App<Model,MModel,Action> =
    {
        unpersist = Unpersist.instance
        threads = threads
        initial = initial
        update = update
        view = view
    }
