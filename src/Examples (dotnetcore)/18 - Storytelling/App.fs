module App

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.Application

open Model
open Provenance

let onNodeClick (cb : Provenance.NodeId -> 'msg) =
    onEvent "onnodeclick" [] (List.head >> Aardvark.UI.Pickler.unpickleOfJson >> Provenance.NodeId >> cb)

let initial =
    let model = BoxSelectionApp.initial in {
        appModel = model
        provenance = model |> Provenance.init
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

        | NodeClick id ->
            model |> restore (Provenance.goto id model.provenance)

        | _ -> 
            model

let view (model : MModel) =

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

    body [ onKeyDown KeyDown; onKeyUp KeyUp; onNodeClick NodeClick ] [
        div [] [
            BoxSelectionApp.view model.appModel
                |> UI.map BoxSelectionAction

            require dependencies (
                onBoot "initChart();" (
                    onBoot' ["provenanceData", provenanceData |> Mod.channel] updateChart (
                        Svg.svg [ clazz "rootSvg"; style "width:100%; height:200px; user-select:none" ] [                                
                            Svg.rect [ style "width:100%; height:100%; fill:#1B1C1E" ]
                        ]
                    )
                )
            )
        ]
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
