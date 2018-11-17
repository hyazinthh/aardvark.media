﻿module ProvenanceApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Model
open Provenance
open Story

[<AutoOpen>]
module private Helpers =

    module Rules =

        let checkMessage (msg : Reduced.Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                    if msg = Reduced.Unknown then
                        Decided tree
                    else
                        Undecided tree 
            )

        let checkStateChanged (state : Reduced.State) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                    if tree.Value.state = state then
                        Decided tree
                    else
                        Undecided tree 
            )

        let checkParent (state : Reduced.State) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                match tree.Parent with
                    | Some p when (p.Value.state = state) ->
                        Decided p
                    | _ ->
                        Undecided tree
            ) 

        let checkChildren (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                let c =
                    tree |> ZTree.filterChildren (fun n ->
                        (n.state = state) && (n.message = Some msg)
                    )
            
                if List.length c > 1 then
                    Log.warn "Multiple identical children in provenance graph."

                match c with
                    | [] -> Undecided tree
                    | t::_ -> Decided t
            ) 

        let coalesceWithCurrent (story : Story) (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                let node = tree.Value

                let coal = 
                    node |> Node.message
                         |> Option.map ((=) msg)
                         |> Option.defaultValue false
                         |> (&&) (story |> Story.isNodeReferenced node |> not)
                         |> (&&) (ZTree.isLeaf tree)                

                match coal with
                    | true ->
                        let v = { node with state = state }
                        Decided (tree |> ZTree.set v)
                    | false ->
                        Undecided tree
            )

        let coalesceWithChild (story : Story)  (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>) =
            input |> Decision.map (fun tree ->
                let c =
                    tree |> ZTree.filterChildren (fun n ->
                                (n.message = Some msg) && (story |> Story.isNodeReferenced n |> not)
                            )
                            |> List.filter ZTree.isLeaf

                if List.length c > 1 then
                    Log.warn "Multiple leaf children to coalesce in provenance graph."

                match c with
                    | [] -> 
                        Undecided tree
                    | t::_ ->
                        let v = { t.Value with state = state }
                        Decided (t |> ZTree.set v)
            )

        let appendNew (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>)=
            input |> Decision.map (fun tree ->
                tree |> ZTree.insert (Node.create state (Some msg)) |> Decided
            )

 [<AutoOpen>]
 module private Events =
    // Fired when a node is clicked 
    let onNodeClick (cb : NodeId -> 'msg) =
        onEvent "onnodeclick" [] (List.head >> Pickler.unpickleOfJson >> NodeId.parse >> cb)

let init (model : AppModel) =
    { tree = Node.create (Reduced.State.create model) None
                |> ZTree.single  }

let restore (model : AppModel) (p : Provenance) =
    Reduced.State.restore model <| Provenance.state p  

let update (story : Story) (msg : ProvenanceAction) (p : Provenance) =
    match msg with
        | Update (s, a) ->
            let state = Reduced.State.create s
            let msg = Reduced.Message.create a

            let t =
                Undecided p.tree
                    |> Rules.checkMessage msg
                    |> Rules.checkStateChanged state
                    |> Rules.checkParent state
                    |> Rules.checkChildren state msg
                    |> Rules.coalesceWithCurrent story state msg
                    |> Rules.coalesceWithChild story state msg
                    |> Rules.appendNew state msg
                    |> Decision.get

            { p with tree = t }

        | Goto id ->
            p.tree |> ZTree.root
                   |> ZTree.find (fun n -> n.id = id)
                   |> fun t -> { p with tree = t }

        | Undo ->
            p.tree |> ZTree.parent
                   |> Option.map (fun t -> { p with tree = t } )
                   |> Option.defaultValue p

let view (p : MProvenance) =
    let dependencies = [
        { kind = Script; name = "d3"; url = "http://d3js.org/d3.v5.min.js" }
        { kind = Stylesheet; name = "provenanceStyle"; url = "Provenance.css" }
        { kind = Script; name = "provenanceScript"; url = "Provenance.js" }
    ]

    let colorSelected = C3d (0.75, 0.95, 0.18);
    let colorHovered = C3d (0.2, 0.8, 0.99);

    let dropShadow (name : string) (color : C3d) =
        let colorMatrix =
            sprintf  "%f 0 0 0 0, 0 %f 0 0 0, 0 0 %f 0 0, 0 0 0 1 0" color.R color.G color.B

        Svg.filter [
            clazz name
            attribute "x" "-50%"
            attribute "y" "-50%"
            attribute "width" "200%"
            attribute "height" "200%"
        ] [
            Svg.feColorMatrix [
                attribute "type" "matrix"
                attribute "result" "whiteOut"
                attribute "in" "SourceGraphic"
                attribute "values" "0 0 0 0 1, 0 0 0 0 1, 0 0 0 0 1, 0 0 0 1 0"
            ]

            Svg.feColorMatrix [
                attribute "type" "matrix"
                attribute "result" "colorOut"
                attribute "in" "whiteOut"
                attribute "color-interpolation-filters" "sRGB"
                attribute "values" colorMatrix
            ]

            Svg.feGaussianBlur [
                attribute "result" "blurOut"
                attribute "in" "colorOut"
                attribute "stdDeviation" "2"
            ]

            Svg.feBlend [
                attribute "in" "SourceGraphic"
                attribute "in2" "blurOut"
                attribute "mode" "normal"
            ]
        ]
        
    let provenanceData = adaptive {
        let! t = p.tree
        
        let json = t.Root.ToJson Provenance.Node.properties
        return sprintf @"{ ""current"" : ""%A"" , ""tree"" : %s }" t.Value.id json
    } 

    let updateChart = "provenanceData.onmessage = function (data) { update(data); };"

    div [ clazz "provenanceView"; onNodeClick Goto ] [
        require dependencies (
            onBoot' ["provenanceData", provenanceData |> Mod.channel] updateChart (
                Svg.svg [ clazz "rootSvg" ] [
                    Svg.defs [] [
                        dropShadow "shadowSelected" colorSelected
                        dropShadow "shadowHovered" colorHovered
                    ]

                    Svg.g [ clazz "linkLayer" ] []
                    Svg.g [ clazz "nodeLayer" ] []
                ]
            )
        )
    ]