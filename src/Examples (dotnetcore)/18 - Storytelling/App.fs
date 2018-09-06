module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application

open Model
open Provenance
open Story

module Html =

    module SemUi =

        let button (icon : string option) (label : string) (enabled : IMod<bool>) (callback : IMod<(unit -> 'a) option>) =

            let attributes = amap {
                let! enabled = enabled
                let! cb = callback

                yield clazz ("ui button " + if enabled then "" else "disabled")

                if cb.IsSome then
                    yield onClick cb.Value
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
        story = Story.empty
        presentation = false
    }

let restore prov model =
    { model with appModel = model.appModel |> Provenance.restore prov
                 provenance = prov }

let restoreSlide model =
    match model.story |> Story.selected |> Slide.content with
        | FrameContent (node, presentation) ->
            model |> Model.setPresentation presentation
                  |> restore (model.provenance |> Provenance.goto node )
        | _ -> 
            model

let update (model : Model) (act : Action) = 
    match act with
        | AppAction a -> 
            let succ = BoxSelectionApp.update model.appModel a
            let prov = model.provenance |> Provenance.update succ a
            let story = model.story |> Story.update prov (AppModel.getPresentation succ)

            { model with appModel = succ
                         provenance = prov }
                |> Model.setStory story

        | KeyDown Keys.Z ->
            match (Provenance.undo model.provenance) with
                | Some prov -> model |> restore prov
                | None -> model

        | AddFrameSlide before ->
            let slide = Slide.frame model.provenance (Model.getPresentation model)
            let story = 
                match before with
                    | None -> model.story |> Story.append slide
                    | Some b -> model.story |> Story.insertBefore slide b

            model |> Model.setStory story

        | RemoveSlide slide ->
            model |> Model.setStory (model.story |> Story.remove slide)

        | KeyDown Keys.R ->
            { model with dockConfig = initial.dockConfig }

        | KeyDown Keys.P ->
            { model with presentation = true }

        | KeyDown Keys.Escape ->
            { model with presentation = false }

        | KeyDown Keys.Right 
        | KeyDown Keys.Enter when model.story |> Story.isActive ->
            model |> Model.setStory (model.story |> Story.forward)
                  |> restoreSlide

        | KeyDown Keys.Left
        | KeyDown Keys.Back when model.story |> Story.isActive ->
            model |> Model.setStory (model.story |> Story.backward)
                  |> restoreSlide

        | NodeClick id ->
            model |> Model.setStory (model.story |> Story.select None)
                  |> restore (model.provenance |> Provenance.goto' id)

        | SlideClick slide ->
            model |> Model.setStory (model.story |> Story.goto slide)
                  |> restoreSlide

        | DeselectSlide ->
            model |> Model.setStory (model.story |> Story.select None)

        | UpdateConfig cfg ->
            { model with dockConfig = cfg }

        | _ -> 
            model

let renderView (model : MModel) =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "overlayStyle"; url = "Overlay.css" }
    ]

    body [ onKeyDown KeyDown; onKeyUp KeyUp ] [
        model.appModel
            |> BoxSelectionApp.renderView
            |> UI.map AppAction

        require (dependencies) (
            Incremental.div (AttributeMap.ofList [ clazz "render overlay"]) (
                alist {
                    let! active = model.story.Current |> Mod.map Story.isActive

                    if active then
                        yield div [clazz "frame"] [
                            i [clazz "huge camera icon"] []
                            i [
                                clazz "huge remove link icon"
                                onClick (fun _ -> DeselectSlide)
                            ] []
                        ]
                }
            )
        )
    ]

let controlsView (model : MModel) =
    body [style "background-color:#1B1C1E"] [
        model.appModel
            |> BoxSelectionApp.controlsView
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

    let addSlideButton (before : Slide option) =
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

    let mkSlide (model : MModel) (selected : bool) (slide : Slide) =
        let atts = [
            clazz ("frame" + if selected then " selected" else "")
            onClick (if selected then (fun _ -> DeselectSlide) else (fun _ -> SlideClick slide))
        ]

        Incremental.div (atts |> AttributeMap.ofList) (
            alist {
                yield img [ attribute "src" "https://upload.wikimedia.org/wikipedia/commons/6/67/SanWild17.jpg" ]

                yield onBoot "disableClickPropagation($('#__ID__'))" (
                    div [
                        clazz "ui icon remove slide button"
                        onClick (fun _ -> RemoveSlide slide)
                    ] [
                        i [clazz "remove icon"] []
                    ]
                )

                let! index = model.story.Current |> Mod.map (Story.findIndex slide)

                yield div [clazz "ui floating blue label"] [
                    text (string (index + 1))
                ]
            }
        )

    (* TODO: Won't need an edit button but probably an apply changes button
                on a per slide basis that allows the current state to be saved in an unselected
                frame *)

            (*let changed = adaptive {
                let! presentation = model.Current |> Mod.map Model.getPresentation

                return match slide.content with
                        | FrameContent (n, p) -> p = presentation
                        | TextContent _ -> false
            }

            yield onBoot "disableClickPropagation($('#__ID__'))" (
                div [ 
                    clazz "ui icon edit slide button"
                    onClick (fun _ -> EditSlide slide)
                ] [
                    i [clazz "edit icon"] []
                ]
            )*)

    body [] [
        require dependencies (
            onBoot "" (
                Incremental.div 
                    (AttributeMap.ofList [ clazz "storyboard" ]) (
                        alist {
                            let! slides = model.story.Current |> Mod.map Story.toList
                            let! selected = model.story.Current |> Mod.map Story.trySelected

                            let cmp slide =
                                match selected with
                                    | None -> false
                                    | Some s -> slide.id = s.id

                            for s in slides do
                                yield onBoot "initPreviewFrame($('#__ID__'))" (
                                    div [clazz "collapsing preview frame"] [ addSlideButton (Some s) ]
                                )
                                yield s |> mkSlide model (cmp s)

                            yield div [clazz "static preview frame"] [
                                yield addSlideButton None
                            ]
                        }
                    )
            )
        )
    ]

let presentationView (model : MModel) =

    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "presentationStyle"; url = "Presentation.css" }
    ]

    let callbackRemove = adaptive {
        let! selected = model.story.Current |> Mod.map Story.trySelected

        return selected |> Option.map (fun s ->
            fun () -> RemoveSlide s
        )
    }

    require (dependencies) (
        body [clazz "ui"] [
            Html.SemUi.accordion "Rendering" "options" true [
                model.appModel |> BoxSelectionApp.renderingControlsView |> UI.map AppAction
            ]     

            Html.SemUi.accordion "Slide" "film" true [

                (* TODO: Probably don't need any of this *)
                Incremental.div (AttributeMap.ofList [clazz "slide status"]) (
                    alist {
                        let! story = model.story.Current

                        let txt = 
                            match story |> Story.trySelected with
                                | None ->
                                    "No slide selected"
                                | Some s -> 
                                    let i = story |> Story.findIndex s
                                    let n = story |> Story.length
                                    sprintf "Slide %d/%d selected" (i + 1) n

                        yield text txt
                    }
                )

                Html.SemUi.button (Some "minus") "Remove"
                                  (model.story.selected |> Mod.map Option.isSome)
                                  callbackRemove
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
