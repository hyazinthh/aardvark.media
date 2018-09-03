namespace Provenance

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open Model
open BoxSelection

type Message =
    | Scale            of BoxId
    | Rotate           of BoxId
    | Translate        of BoxId
    | AddBox
    | RemoveBox
    | Unknown

[<DomainType>]
type State = {
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
    tree : ZTree<Node>
    hasFrames : Node -> bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create = function
        | AppAction.Scale (id, _) -> Scale id
        | AppAction.Rotate (id, _) -> Rotate id
        | AppAction.Translate (id, _) -> Translate id
        | AppAction.AddBox -> AddBox
        | AppAction.RemoveBox -> RemoveBox
        | _ -> Unknown

    let toString = function
        | Scale _       -> "S"
        | Rotate _      -> "R"
        | Translate _   -> "T"
        | AddBox        -> "A"
        | RemoveBox     -> "D"
        | Unknown       -> ""

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : AppModel) =
        { boxes = s.boxes
          nextColor = s.nextColor }
        
    let restore (current : AppModel) (s : State) =
        { current with boxes = s.boxes
                       nextColor = s.nextColor 
                       selectedBoxes = HSet.empty }

    let boxes (s : State) = s.boxes

    let nextColor (s : State) = s.nextColor

    let equal (a : State) (b : State) =
        let boxEqual (x, a) (y, b) =
            x = y && 
            a.color = b.color &&
            a.geometry = b.geometry &&
            a.transform.pose = b.transform.pose

        a.boxes.Count = b.boxes.Count
            |> (&&) (Seq.map2 boxEqual (HMap.toSeq a.boxes) (HMap.toSeq b.boxes) |> Seq.forall id)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Node =

    let create (s : State) (m : Message option) = 
        { id = NodeId (Guid.NewGuid().ToString())
          state = s
          message = m }

    let state (n : Node) = n.state

    let message (n : Node) = n.message

    let id (n : Node) = n.id

    let properties (n : Node) = [ 
        let (NodeId id) = n.id
        yield "id", id

        match n.message with
            | Some m -> yield "msg", (Message.toString m)
            | None -> ()
    ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Provenance =

    let private checkMessage msg input =
        input |> Decision.map (fun (tree : ZTree<Node>) ->
                if msg = Unknown then
                    Decided tree
                else
                    Undecided tree 
        )

    let private checkStateChanged state input =
        input |> Decision.map (fun (tree : ZTree<Node>) ->
                if State.equal tree.Value.state state then
                    Decided tree
                else
                    Undecided tree 
        )

    let private checkParent state input =
        input |> Decision.map (fun (tree : ZTree<Node>) ->
            match tree.Parent with
                | Some p when (State.equal p.Value.state state) ->
                    Decided p
                | _ ->
                    Undecided tree
        ) 

    let private checkChildren state msg input =
        input |> Decision.map (fun (tree : ZTree<Node>) ->
            let c =
                tree |> ZTree.filterChildren (fun n ->
                    (State.equal n.state state) && (n.message = Some msg)
                )
            
            if List.length c > 1 then
                Log.warn "Multiple identical children in provenance graph."

            match c with
                | [] -> Undecided tree
                | t::_ -> Decided t
        ) 

    let private coalesceWithCurrent state msg hasFrames input =
        input |> Decision.map (fun (tree : ZTree<Node>) ->
            let node = tree.Value

            let coal = 
                node |> Node.message
                     |> Option.map ((=) msg)
                     |> Option.defaultValue false
                     |> (&&) (not (hasFrames node))
                     |> (&&) (ZTree.isLeaf tree)                

            match coal with
                | true ->
                    let v = { node with state = state }
                    Decided (tree |> ZTree.set v)
                | false ->
                    Undecided tree
        )

    let private coalesceWithChild state msg input =
        input |> Decision.map (fun (tree : ZTree<Node>) ->
            let c =
                tree |> ZTree.filterChildren (fun n -> n.message = Some msg) 
                     |> List.filter (fun t -> t.IsLeaf)

            if List.length c > 1 then
                Log.warn "Multiple leaf children to coalesce in provenance graph."

            match c with
                | [] -> 
                    Undecided tree
                | t::_ ->
                    let v = { t.Value with state = state }
                    Decided (t |> ZTree.set v)
        )

    let private appendNew state msg input =
        input |> Decision.map (fun tree ->
            tree |> ZTree.insert (Node.create state (Some msg)) |> Decided
        )

    let undo (prov : Provenance) =
        prov.tree
            |> ZTree.parent
            |> Option.map (fun t ->
                { prov with tree = t }
            )

    let goto (node : Node) (prov : Provenance) =
        { prov with tree = prov.tree 
                                |> ZTree.root
                                |> ZTree.find (fun n -> n.id = node.id) }

    let goto' (id : NodeId) (prov : Provenance) =
        { prov with tree = prov.tree 
                                |> ZTree.root
                                |> ZTree.find (fun n -> n.id = id) }
    
    let update (succ : AppModel) (act : AppAction) (prov : Provenance) =

        let state = State.create succ
        let msg = Message.create act

        let t =
            Undecided prov.tree
                |> checkMessage msg
                |> checkStateChanged state
                |> checkParent state
                |> checkChildren state msg
                |> coalesceWithCurrent state msg prov.hasFrames
                |> coalesceWithChild state msg
                |> appendNew state msg
                |> Decision.get

        { prov with tree = t }

    let restore (prov : Provenance) (model : AppModel) =
        State.restore model prov.tree.Value.state

    let init (model : AppModel) =
        { tree = Node.create (State.create model) None
                    |> ZTree.single 
          hasFrames = fun _ -> false }

    let updateNode (f : Node -> Node) (prov : Provenance) =
        { prov with tree = prov.tree |> ZTree.update f}

    let setHasFrames (f : Node -> bool) (prov : Provenance) =
        { prov with hasFrames = f }
