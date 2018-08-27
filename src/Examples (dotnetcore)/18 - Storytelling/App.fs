module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open LensOperators

open Model
open Provenance
open Story

let onNodeClick (cb : NodeId -> 'msg) =
    onEvent "onnodeclick" [] (List.head >> Aardvark.UI.Pickler.unpickleOfJson >> NodeId >> cb)

let initial =
    let model = BoxSelectionApp.initial in {
        appModel = model
        dockConfig = config {
                        content (
                            vertical 1.0 [
                                horizontal 5.0 [
                                    element { id "render"; title "Render View"; weight 5 }

                                    stack 1.0 (Some "controls") [
                                        { id = "controls"; title = Some "Controls"; weight = 1.0; deleteInvisible = None }
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
        story = { slides = PList.empty; current = Index.zero }
    }

let restore prov model =
    { model with appModel = model.appModel |> Provenance.restore prov
                 provenance = prov }

let update (model : Model) (act : Action) = 
    match act with
        | BoxSelectionAction a -> 
            let succ = BoxSelectionApp.update model.appModel a
            let prov = Provenance.update model.provenance succ a 

            { model with appModel = succ
                         provenance = prov }

        | KeyDown Keys.Z ->
            match (Provenance.undo model.provenance) with
                | Some prov -> restore prov model
                | None -> model

        | KeyDown Keys.A ->
            let frame = FrameSlide (model.provenance.tree, [])
            let lens = Model.Lens.story .* Story.Lens.slides
            lens.Update (model, PList.append frame)

        | NodeClick id ->
            model |> restore (Provenance.goto id model.provenance)

        | SlideClick (FrameSlide (t, _)) ->
            model |> restore (Provenance.goto' t model.provenance)

        | _ -> 
            model

let view (model : MModel) =

    page (fun request ->
        match Map.tryFind "page" request.queryParams with
            | Some "render" ->
                body [ onKeyDown KeyDown; onKeyUp KeyUp ] [
                    BoxSelectionApp.renderView model.appModel
                        |> UI.map BoxSelectionAction
                ]
            | Some "controls" ->
                body [style "background-color:#1B1C1E"] [
                    BoxSelectionApp.controlsView model.appModel
                        |> UI.map BoxSelectionAction
                ]
            | Some "provenance" ->
                let dependencies = [
                    { kind = Script; name = "d3"; url = "http://d3js.org/d3.v5.min.js" }
                    { kind = Stylesheet; name = "treeStyle"; url = "Tree.css" }
                    { kind = Script; name = "treeScript"; url = "Tree.js" }
                ]

                let provenanceData = adaptive {
                    let! p = model.provenance.tree
        
                    let (Provenance.NodeId current) = p.Value.id
                    let t = p.Root.ToJson Provenance.Node.properties

                    return sprintf @"{ ""current"" : ""%s"" , ""tree"" : %s }" current t
                } 

                let updateChart = "provenanceData.onmessage = function (data) { update(data); };"

                body [ onNodeClick NodeClick; style "overflow-x:auto; overflow-y:hidden; background-color:#1B1C1E"] [
                    require dependencies (
                        onBoot "initChart();" (
                            onBoot' ["provenanceData", provenanceData |> Mod.channel] updateChart (
                                Svg.svg [ clazz "rootSvg"; style "width:100%; height:100%" ] []
                            )
                        )
                    )
                ]
            | Some "storyboard" ->
                let dependencies = [
                    { kind = Stylesheet; name = "storyboardStyle"; url = "Storyboard.css" }
                ]

                body [] [
                    require dependencies (
                        Incremental.div 
                            (AttributeMap.ofList [ clazz "storyboard" ]) (
                                alist {
                                    for s in model.story.slides do
                                        yield div ([ clazz "frame"; onClick (fun _ -> SlideClick s) ]) [ 
                                            img [ attribute "src" "https://upload.wikimedia.org/wikipedia/commons/6/67/SanWild17.jpg" ] 
                                        ]
                                }
                            )
                    )
                ]
            | Some other ->
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
                ]  
            | None ->
                model.dockConfig |> Mod.force |> Mod.constant |> docking [
                    style "width:100%;height:100%;"
                    onLayoutChanged UpdateConfig
                ]
    )
 
let app : App<Model,MModel,Action> =
    {
        unpersist = Unpersist.instance
        threads = fun model -> model.appModel 
                                    |> BoxSelectionApp.app.threads 
                                    |> ThreadPool.map BoxSelectionAction
        initial = initial
        update = update
        view = view
    }
