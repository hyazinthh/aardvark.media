namespace Provenance

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives

open BoxSelection

type Message =
    | CameraMessage
    | Scale            of BoxId
    | Rotate           of BoxId
    | Translate        of BoxId
    | AddBox
    | RemoveBox
    | Unknown

[<DomainType>]
type State = {
    view : CameraView  
    boxes : hmap<BoxId,VisibleBox>
    nextColor : ColorIndex
}

type NodeId = NodeId of string

[<DomainType>]
type Node = {
    id : NodeId
    state : State
    message : Option<Message>
}

[<DomainType>]
type Provenance = {
    tree : tree<Node>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create = function
        | BoxSelectionAction.CameraMessage _ -> CameraMessage
        | BoxSelectionAction.Scale (id, _) -> Scale id
        | BoxSelectionAction.Rotate (id, _) -> Rotate id
        | BoxSelectionAction.Translate (id, _) -> Translate id
        | BoxSelectionAction.AddBox -> AddBox
        | BoxSelectionAction.RemoveBox -> RemoveBox
        | _ -> Unknown

    let toString = function
        | CameraMessage -> "C"
        | Scale _       -> "S"
        | Rotate _      -> "R"
        | Translate _   -> "T"
        | AddBox        -> "A"
        | RemoveBox     -> "D"
        | Unknown       -> ""

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : BoxSelectionModel) =
        { view = s.camera.view
          boxes = s.boxes
          nextColor = s.nextColor }
        
    let restore (current : BoxSelectionModel) (s : State) =
        { current with camera = { current.camera with view = s.view }
                       boxes = s.boxes
                       nextColor = s.nextColor 
                       selectedBoxes = HSet.empty }

    let view s = s.view

    let boxes s = s.boxes

    let nextColor s = s.nextColor

    let equal a b =
        let boxEqual (x, a) (y, b) =
            x = y && 
            a.color = b.color &&
            a.geometry = b.geometry &&
            a.transform.pose = b.transform.pose

        a.view = b.view
            |> (&&) (a.boxes.Count = b.boxes.Count)
            |> (&&) (Seq.map2 boxEqual (HMap.toSeq a.boxes) (HMap.toSeq b.boxes) |> Seq.forall id)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Node =

    let create s m = 
        { id = NodeId (Guid.NewGuid().ToString())
          state = s
          message = m }

    let state n = n.state

    let message n = n.message

    let id n = n.id

    let properties n = [ 
        let (NodeId id) = n.id
        yield "id", id

        match n.message with
            | Some m -> yield "msg", (Message.toString m)
            | None -> ()
    ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Provenance =

    let private checkMessage msg input =
        input |> Decision.map (fun (tree : tree<Node>) ->
                if msg = Unknown then
                    Decided tree
                else
                    Undecided tree 
        )

    let private checkStateChanged state input =
        input |> Decision.map (fun (tree : tree<Node>) ->
                if State.equal tree.Value.state state then
                    Decided tree
                else
                    Undecided tree 
        )

    let private checkParent state input =
        input |> Decision.map (fun (tree : tree<Node>) ->
            match tree.Parent with
                | Some p when (State.equal p.Value.state state) ->
                    Decided p
                | _ ->
                    Undecided tree
        ) 

    let private checkChildren state msg input =
        input |> Decision.map (fun (tree : tree<Node>) ->
            let c =
                tree |> Tree.filterChildren (fun n ->
                    (State.equal n.state state) && (n.message = Some msg)
                )
            
            if List.length c > 1 then
                Log.warn "Multiple identical children in provenance graph."

            match c with
                | [] -> Undecided tree
                | t::_ -> Decided t
        ) 

    let private coalesceWithCurrent state msg input =
        input |> Decision.map (fun (tree : tree<Node>) ->
            let coal = 
                tree |> Tree.value
                     |> Node.message
                     |> Option.map ((=) msg)
                     |> Option.defaultValue false
                     |> (&&) (Tree.isLeaf tree)                

            match coal with
                | true ->
                    let v = { tree.Value with state = state }
                    Decided (Tree.update v tree)
                | false ->
                    Undecided tree
        )

    let private coalesceWithChild state msg input =
        input |> Decision.map (fun (tree : tree<Node>) ->
            let c =
                tree |> Tree.filterChildren (fun n -> n.message = Some msg) 
                     |> List.filter (fun t -> t.IsLeaf)

            if List.length c > 1 then
                Log.warn "Multiple leaf children to coalesce in provenance graph."

            match c with
                | [] -> 
                    Undecided tree
                | t::_ ->
                    let v = { t.Value with state = state }
                    Decided (Tree.update v t)
        )

    let private appendNew state msg input =
        input |> Decision.map (fun tree ->
            tree |> Tree.insert (Node.create state (Some msg)) |> Decided
        )

    let undo prov =
        prov.tree
            |> Tree.parent
            |> Option.map (fun t ->
                { prov with tree = t }
            )

    let goto id prov =
        { prov with tree = prov.tree 
                                |> Tree.root
                                |> Tree.find (fun n -> n.id = id) }
    
    let update prov succ act =

        let state = State.create succ
        let msg = Message.create act

        let t =
            Undecided prov.tree
                |> checkMessage msg
                |> checkStateChanged state
                |> checkParent state
                |> checkChildren state msg
                |> coalesceWithCurrent state msg
                |> coalesceWithChild state msg
                |> appendNew state msg
                |> Decision.get

        { prov with tree = t }

    let restore prov model =
        State.restore model prov.tree.Value.state

    let init model =
        { tree = Node.create (State.create model) None
                    |> Tree.single }
