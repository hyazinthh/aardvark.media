module App

open System.Net
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open Aardvark.Service.Server

open Model
open Provenance
open Story

module Model = 

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

        [<AutoOpen>]
        module private Helpers =
            // Updates the currently selected slide (if not None)
            // with changes from model
            let saveChanges (model : Model) =

                let updateSelected (sel : Slide) =
                    match sel.content with
                        | TextContent _ -> sel
                        | FrameContent _ ->
                            Slide.Lens.content.Set (sel, model |> Model.getFrame)

                let updateStory (s : Story) =
                    match s.selected with
                        | None -> s
                        | Some sel ->
                            s |> Story.saveChanges (sel |> updateSelected)

                model.story |> updateStory

        // Sets the story and updates the provenance
        // accordingly
        let update (f : Story -> Story) (model : Model) =
            let story = model |> saveChanges |> f
            
            model |> Lens.set Model.Lens.story story
                  |> Lens.set Model.Lens.provenance (model.provenance |> Story.Provenance.update story)

        // Restores the model from the selected slide
        let restore (model : Model) =
            match model.story |> Story.selected |> Slide.content with
                | FrameContent (node, presentation) ->
                    model |> Model.setPresentation presentation
                          |> Provenance.update (Provenance.goto node)
                          |> Provenance.restore
                | _ -> 
                    model
        
        // Requests a thumbnail for the given slide
        let requestThumbnail (slide : Slide) (model : Model) =
            model |> Lens.update Model.Lens.thumbnailRequests (HSet.add slide)

        // Remove a thumbnail request
        let removeThumbnailRequest (slide : Slide) (model : Model) =
            model |> Lens.update Model.Lens.thumbnailRequests (HSet.remove slide)
            


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
        thumbnailRequests = HSet.empty
    }

let update (model : Model) (act : Action) = 
    match act with
        | AppAction a -> 
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
                  |> Model.Story.requestThumbnail slide

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
                  |> Model.Story.restore

        | KeyDown Keys.Left
        | KeyDown Keys.Back when model.story |> Story.isActive ->
            model |> Model.Story.update Story.backward
                  |> Model.Story.restore

        | NodeClick id ->
            model |> Model.Story.update (Story.select None)
                  |> Model.Provenance.update (Provenance.goto' id)
                  |> Model.Provenance.restore

        | SlideClick id ->
            model |> Model.Story.update (Story.selectById (Some id))
                  |> Model.Story.restore

        | DeselectSlide ->
            model |> Model.Story.update (Story.select None)

        | ThumbnailUpdated slide ->
            if model.story |> Story.contains slide then
                model |> Model.Story.update (Story.set slide slide)
            else
                model
            |> Model.Story.removeThumbnailRequest slide

        | UpdateConfig cfg ->
            { model with dockConfig = cfg }

        | _ -> 
            model


let threads (model : Model) =
    
    // Thread pool for actual application
    let appThreads = model.appModel |> BoxSelectionApp.app.threads 
                                    |> ThreadPool.map AppAction

    // Threads for creating thumbnails
    let thumbnailRequests =

        let thumbnailProc slide =
            proclist {
                do! Async.SwitchToNewThread ()
                yield ThumbnailUpdated { 
                    slide with thumbnail = Thumbnail.create ()
                }
            }

        model.thumbnailRequests
            |> HSet.fold (fun p s ->
                p |> ThreadPool.add (string s.id) (thumbnailProc s)
            ) ThreadPool.empty

    // Thread for keeping the thumbnail of the selected
    // slide updated
    let thumbnailUpdater =

        let updateProc (selected : Slide) =
            let rec proc () =
                proclist {
                    yield ThumbnailUpdated {
                        selected with thumbnail = Thumbnail.create ()
                    }

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
                    |> Option.map spawn
                    |> Option.defaultValue ThreadPool.empty

    [appThreads; thumbnailRequests; thumbnailUpdater]
        |> ThreadPool.unionMany


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
                    let! active = model.story.selected |> Mod.map Option.isSome

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
        
        let t = p.Root.ToJson Provenance.Node.properties
        return sprintf @"{ ""current"" : ""%s"" , ""tree"" : %s }" (string p.Value.id) t
    } 

    let updateChart = "provenanceData.onmessage = function (data) { update(data); };"

    body [ Events.onNodeClick NodeClick ] [
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
