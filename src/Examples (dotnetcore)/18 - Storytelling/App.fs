module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
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

        | SlideClick t when t.IsSome ->
            model |> restore (Provenance.set t.Value model.provenance)

        | _ -> 
            model

let view (model : MModel) =

    let dependencies = [ 
            { kind = Script; name = "d3"; url = "http://d3js.org/d3.v5.min.js" }
            { kind = Stylesheet; name = "treeStyle"; url = "Tree.css" }
            { kind = Script; name = "treeScript"; url = "Tree.js" }
            { kind = Stylesheet; name = "storyboardStyle"; url = "Storyboard.css" }
        ]

    let provenanceData = adaptive {
        let! p = model.provenance.tree
        
        let (Provenance.NodeId current) = p.Value.id
        let t = p.Root.ToJson Provenance.Node.properties

        return sprintf @"{ ""current"" : ""%s"" , ""tree"" : %s }" current t
    } 

    let updateChart = "provenanceData.onmessage = function (data) { update(data); };"

    body [ onKeyDown KeyDown; onKeyUp KeyUp; onNodeClick NodeClick ] [
        div [style "width:100%; height:70%; overflow:hidden"] [
            BoxSelectionApp.view model.appModel
                |> UI.map BoxSelectionAction
        ]

        div [style "width:100%; height:15%; overflow-x:auto; overflow-y:hidden; background-color:#1B1C1E"] [
            require dependencies (
                onBoot "initChart();" (
                    onBoot' ["provenanceData", provenanceData |> Mod.channel] updateChart (
                        Svg.svg [ clazz "rootSvg"; style "width:100%; height:100%" ] []
                    )
                )
            )
        ]

        Incremental.div 
            (AttributeMap.ofList [ clazz "storyboard"; style "height:15%" ]) (
                alist {
                    for s in model.story.slides do
                        let click = 
                            onClick (fun _ ->
                                SlideClick ( match s with
                                                | FrameSlide (t, _) -> Some t
                                                | _ -> None )
                            )

                        yield div ([ clazz "frame"; click ]) [ 
                            img [ attribute "src" "https://upload.wikimedia.org/wikipedia/commons/6/67/SanWild17.jpg" ] 
                        ]
                }
            )
    ]
 
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
