﻿module App

open System.Net
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Generic
open Aardvark.UI.Primitives
open Aardvark.UI.Animation
open Aardvark.Application
open Aardvark.Service.Server

open Model
open Provenance
open Story
open Annotations

module Model =

    module Annotations =
        // Updates the annotations
        let update (msg : AnnotationAction) (model : Model) =
            let sel = Option.get model.story.selected

            let content =
                match sel.content with
                    | FrameContent (n, p, a) -> 
                        FrameContent (n, p, a |> Annotations.update msg)               
                    | x -> x

            let sel = { sel with content = content }
            model |> Lens.update Model.Lens.story (Story.set sel sel)

    module Animation =
        // Removes all animations
        let removeAll (model : Model) =
            let l = Model.Lens.animation |. Animation.Lens.model |. AnimationModel.Lens.animations
            model |> Lens.set l PList.empty

        // Updates the animations
        let update (msg : AnimationAction) (model : Model) =
            let updateCamera (prev : Model) (model : Model) =

                if Model.isAnimating model then
                    model |> Model.setView model.animation.model.cam
                else
                    // TODO: We need to do this cause the animation system is buggy, see below
                    if Model.isAnimating prev then
                        model |> Model.setView model.animation.savedView
                    else
                        model

            model |> Lens.set (Model.Lens.animation |. Animation.Lens.model) (msg |> AnimationApp.update model.animation.model)
                  |> updateCamera model

        // Sets the current view for the animations and saves the
        // current / destination view. We need to save the final view since
        // the animation system is buggy and does not actually return the final state.
        // Instead it is set manually once the animation is finished.
        let setView (view : CameraView) (model : Model) =
            model |> Lens.set (Model.Lens.animation |. Animation.Lens.model |. AnimationModel.Lens.cam) view
                  |> Lens.set (Model.Lens.animation |. Animation.Lens.savedView) (Model.getView model)

    module Camera =
        // Initializes a camera transition from src to dest
        let animate (duration : RelativeTime) (src : Model) (dest : Model) =
            let srcView = src |> Model.getView
            let destView = dest |> Model.getView

            if Reduced.CameraView.equal srcView destView then
                dest
            else
                let animation = CameraAnimations.interpolate destView duration ""

                dest |> Animation.setView srcView
                     |> Animation.removeAll
                     |> Animation.update (animation |> PushAnimation)

    module Provenance =
        // Updates the provenance unless the mapping returns None
        let updateMaybe (f : Provenance -> Provenance option) (model : Model) =
            match (model.provenance |> f) with
                | None -> model
                | Some p ->
                    { model with provenance = p }

        // Updates the provenance
        let update (f : Provenance -> Provenance) (model : Model) =
            updateMaybe (f >> Some) model

        // Restores the model from provenance
        let restore (model : Model) =
            { model with appModel = model.appModel |> Provenance.restore model.provenance }

        module Story =
            // Updates the provenance with the story
            let update (model : Model) =
                model |> Lens.update Model.Lens.provenance (Story.Provenance.update model.story)

    module Story =
        [<AutoOpen>]
        module private Helpers =
            // Updates the currently selected slide (if not None)
            // with changes from model
            let saveChanges (model : Model) =

                let updateSelected (sel : Slide) =
                    match sel.content with
                        | TextContent _ -> sel
                        | FrameContent (_, _, a) ->
                            let n = model.provenance.tree.Value
                            let p = Model.getPresentation model
                            sel |> Lens.set Slide.Lens.content (FrameContent (n, p, a))

                let updateStory (s : Story) =
                    match s.selected with
                        | None -> s
                        | Some sel ->
                            s |> Story.saveChanges (sel |> updateSelected)

                model |> Lens.update Model.Lens.story updateStory

        // Sets the story and updates the provenance
        // accordingly
        let update (f : Story -> Story) (model : Model) =
            model |> if Model.isAnimating model then id else saveChanges
                  |> Lens.update Model.Lens.story f
                  |> Provenance.Story.update

        // Restores the model from the selected slide
        let restore (animate : bool) (model : Model) =
            match model.story |> Story.selected |> Slide.content with
                | FrameContent (node, presentation, _) ->
                    model |> Model.setPresentation presentation
                          |> Provenance.update (Provenance.goto node)
                          |> Provenance.restore
                          |> if animate then Camera.animate 0.5 model else id
                | _ -> 
                    model
        
        // Requests a thumbnail for the given slide
        let requestThumbnail (id : SlideId) (model : Model) =
            model |> Lens.update Model.Lens.thumbnailRequests (HSet.add id)

        // Remove a thumbnail request
        let removeThumbnailRequest (id : SlideId) (model : Model) =
            model |> Lens.update Model.Lens.thumbnailRequests (HSet.remove id)
            
 module Events =
    // Fired when a node is clicked 
    let onNodeClick (cb : NodeId -> 'msg) =
        onEvent "onnodeclick" [] (List.head >> Pickler.unpickleOfJson >> NodeId.parse >> cb)

    // Fired when a slide is dragged and dropped
    let onSlideMove (cb : SlideId * SlideId option * SlideId option -> 'msg) =
        onEvent "onslidemove" [] (fun args ->
            let x = args |> List.toArray|> Array.map (Pickler.unpickleOfJson >> SlideId.tryParse)
            cb (x.[0].Value, x.[1], x.[2])
        )

module Thumbnail =

    [<AutoOpen>]
    module private Helpers = 

        let baseAddress = "http://localhost:4321"

        let controlId = lazy (
            use wc = new WebClient ()
            let result = wc.DownloadString (sprintf "%s/rendering/stats.json" baseAddress)
            let stats : ClientStatistics list =
                   result |> Pickler.unpickleOfJson           

            stats.Head.name
        )

    // Creates a thumbnail by taking a screenshot
    let create () =
        use wc = new WebClient ()
        wc.DownloadData (sprintf "%s/rendering/screenshot/%s?w=256&h=196&samples=8" baseAddress controlId.Value)
            |> Thumbnail.create

let initial =
    let model = BoxSelectionApp.initial in {
        appModel = model
        dockConfig = config {
                        content (
                            vertical 1.0 [
                                horizontal 5.0 [
                                    element { id "render"; title "Render View"; isCloseable false; weight 3 }

                                    stack 1.0 (Some "controls") [
                                        { id = "controls"; title = Some "Controls"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                                        { id = "presentation"; title = Some "Presentation"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                                    ]
                                ]

                                stack 1.0 (Some "provenance") [
                                    { id = "provenance"; title = Some "History"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                                    { id = "storyboard"; title = Some "Storyboard"; weight = 1.0; deleteInvisible = None; isCloseable = Some true }
                                ]
                            ]
                        )
                        appName "Box Selection"
                        useCachedConfig false
                    }
        provenance = model |> Provenance.init
        story = Story.empty
        presentation = false
        thumbnailRequests = HSet.empty
        animation = { model = { cam = model.camera.view; animation = Animate.On; animations = PList.empty }
                      savedView = model.camera.view }
    }

let update (model : Model) (act : Action) = 
    match act with
        | AnimationAction a ->
            model |> Model.Animation.update a

        | AnnotationAction a ->
            model |> Model.Annotations.update a

        | AppAction a when not (Model.isAnimating model) ->
            let succ = BoxSelectionApp.update model.appModel a
            let prov = model.provenance |> Provenance.update succ a
            
            model |> Lens.set Model.Lens.appModel succ
                  |> Lens.set Model.Lens.provenance prov

        | KeyDown Keys.Z ->
            model |> Model.Provenance.updateMaybe Provenance.undo

        | AddFrameSlide before ->
            let slide = Slide.frame model.provenance (Model.getPresentation model) Thumbnail.empty
            let story = 
                match before with
                    | None -> Story.append slide
                    | Some id -> Story.insertBeforeById slide id

            model |> Model.Story.update story
                  |> Model.Story.requestThumbnail slide.id

        | RemoveSlide id ->
            model |> Model.Story.update (Story.removeById id)

        | MoveSlide (id, l, r) ->
            let story = 
                if l.IsSome then
                    Story.moveAfterById id l.Value
                else
                    Story.moveBeforeById id r.Value

            model |> Model.Story.update story

        | KeyDown Keys.R ->
            { model with dockConfig = initial.dockConfig }

        | KeyDown Keys.P ->
            { model with presentation = true }

        | KeyDown Keys.Escape ->
            { model with presentation = false }

        | KeyDown Keys.Right 
        | KeyDown Keys.Enter when model.story |> Story.isActive ->
            model |> Model.Story.update Story.forward
                  |> Model.Story.restore true

        | KeyDown Keys.Left
        | KeyDown Keys.Back when model.story |> Story.isActive ->
            model |> Model.Story.update Story.backward
                  |> Model.Story.restore true

        | NodeClick id ->
            model |> Model.Story.update (Story.select None)
                  |> Model.Provenance.update (Provenance.goto' id)
                  |> Model.Provenance.restore

        | SlideClick id ->
            model |> Model.Story.update (Story.selectById (Some id))
                  |> Model.Story.restore true

        | DeselectSlide ->
            model |> Model.Story.update (Story.select None)

        | ThumbnailUpdated (id, t) ->
            let slide = model.story |> Story.tryFindById id

            match slide with
                | Some s -> 
                    model |> Model.Story.update (Story.set s { s with thumbnail = t })
                | None -> 
                    model
            |> Model.Story.removeThumbnailRequest id

        | UpdateConfig cfg ->
            { model with dockConfig = cfg }

        | _ -> 
            model

let threads (model : Model) =
    
    // Thread pool for actual application
    let appThreads = model.appModel |> BoxSelectionApp.app.threads 
                                    |> ThreadPool.map AppAction

    // Thread pool for animations
    let animationThreads = model.animation.model |> AnimationApp.ThreadPool.threads
                                                 |> ThreadPool.map AnimationAction

    // Threads for creating thumbnails
    let thumbnailRequests =

        let thumbnailProc (id : SlideId) =
            proclist {
                do! Async.SwitchToNewThread ()
                yield ThumbnailUpdated (id, Thumbnail.create ())
            }

        model.thumbnailRequests
            |> HSet.fold (fun p id ->
                p |> ThreadPool.add (string id) (thumbnailProc id)
            ) ThreadPool.empty

    // Thread for keeping the thumbnail of the selected
    // slide updated
    let thumbnailUpdater =

        let updateProc (selected : Slide) =
            let rec proc () =
                proclist {
                    yield ThumbnailUpdated (selected.id, Thumbnail.create ())

                    do! Async.Sleep 1000
                    yield! proc ()
                }
            
            proclist {
                do! Async.SwitchToNewThread ()
                yield! proc ()
            }
    
        let spawn (selected : Slide) =
            selected |> updateProc |> ThreadPool.single ("u" + (string selected.id))

        // Only run if there is actually a frame selected
        model.story |> Story.trySelected
                    |> Option.filter Slide.isFrame
                    |> Option.filter (fun _ -> model |> Model.isAnimating |> not)
                    |> Option.map spawn
                    |> Option.defaultValue ThreadPool.empty

    [appThreads; animationThreads; thumbnailRequests; thumbnailUpdater]
        |> ThreadPool.unionMany


let renderView (model : MModel) =
    let overlay (a : MAnnotations) =
        Incremental.div (AttributeMap.ofList [clazz "frame"]) (
            alist {
                yield i [clazz "huge camera icon"] []
                yield i [
                    clazz "huge remove link icon"
                    onClick (fun _ -> DeselectSlide)
                ] []

                let! focus = a.focus |> Mod.map Option.isSome
                let! hidden = a.show |> Mod.map not

                yield div [clazz "annotation menu"] [
                    div [
                        clazz ("ui icon toggle button" + if hidden then "" else " active")
                        onClick (fun _ -> Toggle |> AnnotationAction)
                    ] [
                        i [clazz "comments icon"] []
                    ]

                    div [
                        yield clazz "ui vertical buttons"
                        if hidden then
                            yield style "display: none"
                    ] [
                        div [clazz ("ui icon button" + if focus then "" else " disabled")] [
                            i [clazz "font icon"] []
                        ]
                        div [clazz ("ui icon button" + if focus then "" else " disabled")] [
                            i [clazz "flag icon"] []
                        ]
                        div [
                            clazz "ui icon button"
                            onClick (fun _ -> Add |> AnnotationAction)
                        ] [
                            i [clazz "add icon"] []
                        ]
                    ]

                ]
            }
        )

    let mkAnnotation (a : MAnnotation) =

        let setData = "widthData.onmessage = function (data) { setWidth($('#__ID__'), data); };" +
                      "positionData.onmessage = function (data) { setPosition($('#__ID__'), data); };"

        onBoot "initLabel($('#__ID__'))" (
            onBoot' [ "widthData", a.label.width |> Mod.channel
                      "positionData", a.label.position |> Mod.channel ] setData (
                let atts = amap {
                    let! id = a.id
     
                    yield clazz "label"
                    yield onLabelMoved (fun p -> (id, p) |> LabelMoved |> AnnotationAction)
                    yield onLabelResized (fun w -> (id, w) |> LabelResized |> AnnotationAction)
                }
            
                Incremental.div (AttributeMap.ofAMap atts) (
                    alist {
                        let! id = a.id

                        yield div [clazz "grabber"] []

                        yield div [clazz "ui icon move button"] [
                            i [clazz "move icon"] []
                        ]

                        yield div [
                            clazz "ui icon remove button"
                            onClick (fun _ -> id |> Remove |> AnnotationAction)
                        ] [
                            i [clazz "remove icon"] []
                        ]

                        yield textarea [
                            attribute "rows" "1"
                            attribute "placeholder" "Add text here..."
                            onChange (fun s -> (id, s) |> LabelChanged |> AnnotationAction)
                            onFocus (fun _ -> id |> Focus |> AnnotationAction)
                            onBlur (fun _ -> Blur |> AnnotationAction)
                        ] a.label.text
                    }
                )
            )
        )
        

    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "overlayStyle"; url = "Overlay.css" }
        { kind = Stylesheet; name = "annotationsStyle"; url = "Annotations.css" }
        { kind = Script; name = "annotationsScript"; url = "Annotations.js" }
    ]

    // TODO: Adding keyboard events here breaks input events
    // for the text areas of the annotations
    body [ (*onKeyDown KeyDown; onKeyUp KeyUp*) ] [
        model.appModel
            |> BoxSelectionApp.renderView
            |> UI.map AppAction

        require (dependencies) (
            Annotations.init (
                Incremental.div (AttributeMap.ofList [ clazz "render overlay"]) (
                    alist {
                        let! selected = model.story.selected

                        if selected.IsSome then
                            let! cont = selected.Value.content

                            match cont with
                                | MFrameContent (_, _, annotations) ->
                                    let! show = annotations.show

                                    if show then
                                        yield! annotations.list |> AList.map mkAnnotation

                                    yield overlay annotations
                                | _ -> ()
                    }
                )
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
        
        let t = p.Root.ToJson Provenance.Node.properties
        return sprintf @"{ ""current"" : ""%s"" , ""tree"" : %s }" (string p.Value.id) t
    } 

    let updateChart = "provenanceData.onmessage = function (data) { update(data); };"

    body [ Events.onNodeClick NodeClick ] [
        require dependencies (
            onBoot "initChart()" (
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

    let mkCollapsingFrame (model : MModel) (slide : MSlide) =
        alist {
            let! id = slide.id

            // TODO: Optimize! This depends on the whole story record!
            let! slide = slide.Current
            let! prev = model.story.Current |> Mod.map (fun s ->
                            s |> Story.leftOf slide |> Option.map (Slide.id >> string)
                        )

            yield onBoot "initCollapsingFrame($('#__ID__'))" (
                div [
                    yield clazz "collapsing preview frame"
                    yield attribute "right" (string id)
                    if prev.IsSome then 
                        yield attribute "left" prev.Value
                ] [ addSlideButton (Some id) ]
            )
        }

    let mkStaticFrame (model : MModel) =
        alist {
            // TODO: Optimize! This depends on the whole story record!
            let! last = model.story.Current |> Mod.map (fun s ->
                            s |> Story.last |> Option.map (Slide.id >> string)
                        )

            yield onBoot "setupDropEvents($('#__ID__'))" (
                div [
                    yield clazz "static preview frame"
                    if last.IsSome then
                        yield attribute "left" last.Value
                ] [ yield addSlideButton None ]
            )
        }
        

    let mkSlide (model : MModel) (slide : MSlide)  =
        alist {
            let! id = slide.id
            let! thumbnail = slide.thumbnail

            // TODO: Optimize! This depends on the whole story record!
            let! slide = slide.Current
            let! index = model.story.Current |> Mod.map (Story.findIndex slide)

            let! selected = adaptive {
                let! sel = model.story.selected

                match sel with
                    | None -> return false
                    | Some x -> return! x.id |> Mod.map ((=) id)
            }

            let atts = [
                clazz ("frame" + if selected then " selected" else "")
                attribute "slide" (string id)
                onClick (if selected then (fun _ -> DeselectSlide) else (fun _ -> SlideClick id))
            ]

            let boot = "setupDragEvents($('#__ID__')); " +
                       (sprintf "setupThumbnail($('#__ID__'), '%A')" thumbnail)

            yield onBoot boot (
                div atts [
                    yield onBoot "disableClickPropagation($('#__ID__'))" (
                        div [
                            clazz "ui icon remove slide button"
                            onClick (fun _ -> RemoveSlide id)
                        ] [
                            i [clazz "remove icon"] []
                        ]
                    )

                    yield div [clazz "ui floating blue label"] [
                        text (string (index + 1))
                    ]
                ]
            )
        }

    body [] [
        require dependencies (
            Incremental.div 
                (AttributeMap.ofList [ clazz "storyboard"; Events.onSlideMove MoveSlide ]) (
                    alist {
                        for s in model.story.slides do
                            yield! s |> mkCollapsingFrame model
                            yield! s |> mkSlide model

                        yield! mkStaticFrame model
                    }
                )
        )
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
