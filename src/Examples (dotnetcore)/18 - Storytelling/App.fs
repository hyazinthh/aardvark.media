module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open Model
open Provenance
open Story

module Html =

    module SemUi =

        let button (icon : string option) (label : string) (enabled : IMod<bool>) (callback : IMod<unit -> 'a>) =

            let attributes = amap {
                let! enabled = enabled
                let! cb = callback

                let cl = "ui button " + if enabled then "" else "disabled"

                yield! [clazz cl; onClick cb]
            }

            Incremental.div (attributes |> AttributeMap.ofAMap) (
                alist {
                    if icon.IsSome then 
                        yield i [clazz ("icon " + icon.Value)] []
                    yield text label
                }
            )

let onNodeClick (cb : NodeId -> 'msg) =
    onEvent "onnodeclick" [] (List.head >> Aardvark.UI.Pickler.unpickleOfJson >> NodeId >> cb)

let initial =
    let model = BoxSelectionApp.initial in {
        appModel = model
        dockConfig = config {
                        content (
                            vertical 1.0 [
                                horizontal 5.0 [
                                    element { id "render"; title "Render View"; weight 3 }

                                    stack 1.0 (Some "controls") [
                                        { id = "controls"; title = Some "Controls"; weight = 1.0; deleteInvisible = None }
                                        { id = "presentation"; title = Some "Presentation"; weight = 1.0; deleteInvisible = None }
                                    ]
                                ]

                                stack 1.0 (Some "provenance") [
                                    { id = "provenance"; title = Some "History"; weight = 1.0; deleteInvisible = None }
                                    { id = "storyboard"; title = Some "Storyboard"; weight = 1.0; deleteInvisible = None }
                                ]
                            ]
                        )
                        appName "Box Selection"
                        useCachedConfig false
                    }
        provenance = model |> Provenance.init
        story = { slides = ZList.empty; selected = None }
        presentation = false
    }

let restore prov model =
    { model with appModel = model.appModel |> Provenance.restore prov
                 provenance = prov }

let restoreSlide model =
    match model.story |> Story.selected |> Slide.content with
        | FrameContent (node, view, rendering) ->
            model |> Model.setView view 
                  |> Model.setRendering rendering
                  |> restore (Provenance.goto node model.provenance)
        | _ -> 
            model

let update (model : Model) (act : Action) = 
    match act with
        | AppAction a -> 
            let succ = BoxSelectionApp.update model.appModel a
            let prov = model.provenance |> Provenance.update succ a 

            { model with appModel = succ
                         provenance = prov }

        | KeyDown Keys.Z ->
            match (Provenance.undo model.provenance) with
                | Some prov -> restore prov model
                | None -> model

        | AddFrameSlide before ->
            let slide = Slide.frame model.provenance (Model.getView model) (Model.getRendering model)
            let story = 
                match before with
                    | None -> model.story |> Story.append slide
                    | Some b -> model.story |> Story.insertBefore' slide b

            { model with story = story
                         provenance = model.provenance |> Provenance.setHasFrames (Story.hasFrames story) }

        | RemoveSlide id ->
            let story = model.story |> Story.remove' id
            { model with story = story
                         provenance = model.provenance |> Provenance.setHasFrames (Story.hasFrames story) }

        | KeyDown Keys.R ->
            { model with dockConfig = initial.dockConfig }

        | KeyDown Keys.P ->
            { model with presentation = true }

        | KeyDown Keys.Escape ->
            { model with presentation = false }

        | KeyDown Keys.Right 
        | KeyDown Keys.Enter when model.story |> Story.isActive ->
            { model with story = Story.forward model.story } 
                |> restoreSlide

        | KeyDown Keys.Left
        | KeyDown Keys.Back when model.story |> Story.isActive ->
            { model with story = Story.backward model.story } 
                |> restoreSlide

        | NodeClick id ->
            { model with story = model.story |> Story.select None }
                |> restore (Provenance.goto' id model.provenance)

        | SlideClick id ->
            { model with story = model.story |> Story.goto' id }
                |> restoreSlide

        | UpdateConfig cfg ->
            { model with dockConfig = cfg }

        | _ -> 
            model

let renderView (model : MModel) =
    body [ onKeyDown KeyDown; onKeyUp KeyUp ] [
        BoxSelectionApp.renderView model.appModel
            |> UI.map AppAction
    ]

let controlsView (model : MModel) =
    body [style "background-color:#1B1C1E"] [
        BoxSelectionApp.controlsView model.appModel
            |> UI.map AppAction
    ]

let provenanceView (model : MModel) =
    let dependencies = [
        { kind = Script; name = "d3"; url = "http://d3js.org/d3.v5.min.js" }
        { kind = Stylesheet; name = "provenanceStyle"; url = "Provenance.css" }
        { kind = Script; name = "provenanceScript"; url = "Provenance.js" }
    ]

    let provenanceData = adaptive {
        let! p = model.provenance.tree
        
        let (Provenance.NodeId current) = p.Value.id
        let t = p.Root.ToJson Provenance.Node.properties

        return sprintf @"{ ""current"" : ""%s"" , ""tree"" : %s }" current t
    } 

    let updateChart = "provenanceData.onmessage = function (data) { update(data); };"

    body [ onNodeClick NodeClick ] [
        require dependencies (
            onBoot "initChart();" (
                onBoot' ["provenanceData", provenanceData |> Mod.channel] updateChart (
                    Svg.svg [ clazz "rootSvg"; style "width:100%; height:100%" ] []
                )
            )
        )
    ]

let storyboardView (model : MModel) =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "storyboardStyle"; url = "Storyboard.css" }
        { kind = Script; name = "storyboardScript"; url = "Storyboard.js" }
    ]

    let addSlideButton (before : SlideId option) =
        onBoot "initAddButton($('#__ID__'))" (
            div [clazz "add button"] [
                div [clazz "ui icon button first"] [
                    i [clazz "add icon"] []
                ]
                div [clazz "ui vertical buttons second"] [
                    div [clazz "ui button"; onClick (fun _ -> AddFrameSlide before)] [
                        i [clazz "camera icon"] []
                        text "Frame"                        
                    ]
                    div [clazz "ui button"; onClick (fun _ -> AddTextSlide before)] [
                        i [clazz "list icon"] []
                        text "Text"
                    ]
                ]
            ]
        )

    let mkSlide sel slide =
        let atts = [
            clazz ("frame" + if sel then " selected" else "")
            onClick (fun _ -> SlideClick slide.id)
        ]

        div atts [
            img [ attribute "src" "https://upload.wikimedia.org/wikipedia/commons/6/67/SanWild17.jpg" ]

            onBoot "initRemoveButton($('#__ID__'))" (
                div [ 
                    clazz "ui icon remove button" 
                    onClick (fun _ -> RemoveSlide slide.id)
                ] [
                    i [clazz "remove icon"] []
                ]
            )
        ]

    body [] [
        require dependencies (
            onBoot "" (
                Incremental.div 
                    (AttributeMap.ofList [ clazz "storyboard" ]) (
                        alist {
                            let! slides = model.story.slides
                        
                            (* TODO: can this be done more cleanly? *)
                            let! id = adaptive {
                                let! s = model.story.selected
                                match s with
                                    | None -> return SlideId ""
                                    | Some s -> return! s.id
                            }

                            for s in slides |> ZList.toList do
                                yield onBoot "initPreviewFrame($('#__ID__'))" (
                                    div [clazz "collapsing preview frame"] [ addSlideButton (Some s.id) ]
                                )
                                yield mkSlide (s.id = id) s

                            yield div [clazz "static preview frame"] [
                                yield addSlideButton None
                            ]
                        }
                    )
            )
        )
    ]

let presentationView (model : MModel) =

    let callbackRemove = adaptive {
        let! sel = model.story.selected

        if sel.IsNone then
            return fun () -> RemoveSlide (SlideId "")
        else
            let! id = sel.Value.id
            return fun () -> RemoveSlide id
    }

    require (Html.semui) (
        body [clazz "ui"; style "background-color:#1B1C1E"] [
            Html.SemUi.accordion "Rendering" "options" true [
                model.appModel |> BoxSelectionApp.renderingControlsView |> UI.map AppAction
            ]     

            Html.SemUi.accordion "Slides" "film" true [

                Html.SemUi.button (Some "minus") "Remove" 
                                    (model.story.selected |> Mod.map Option.isSome)
                                    callbackRemove

                (*div [clazz "ui button"] [
                    i [clazz "check icon"] []
                    text "Apply"
                ]*)
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
        threads = fun model -> model.appModel 
                                    |> BoxSelectionApp.app.threads 
                                    |> ThreadPool.map AppAction
        initial = initial
        update = update
        view = view
    }
