namespace Provenance

open System
open Aardvark.Base
open Aardvark.Base.Incremental

open Model

type NodeId = NodeId of string

[<DomainType>]
type Node = {
    id : NodeId
    state : Reduced.State
    message : Option<Reduced.Message>
}

[<DomainType>]
type Provenance = {
    tree : ZTree<Node>
    persistForStory : Node -> bool    // Returns if changing the node would break the story
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Node =

    let create (s : Reduced.State) (m : Reduced.Message option) =
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
            | Some m -> yield "msg", (Reduced.Message.toString m)
            | None -> ()
    ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Provenance =

    let private checkMessage (msg : Reduced.Message) (input : Decision<Node ztree>) =
        input |> Decision.map (fun tree ->
                if msg = Reduced.Unknown then
                    Decided tree
                else
                    Undecided tree 
        )

    let private checkStateChanged (state : Reduced.State) (input : Decision<Node ztree>) =
        input |> Decision.map (fun tree ->
                if tree.Value.state = state then
                    Decided tree
                else
                    Undecided tree 
        )

    let private checkParent (state : Reduced.State) (input : Decision<Node ztree>) =
        input |> Decision.map (fun tree ->
            match tree.Parent with
                | Some p when (p.Value.state = state) ->
                    Decided p
                | _ ->
                    Undecided tree
        ) 

    let private checkChildren (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>) =
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

    let private coalesceWithCurrent (prov : Provenance) (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>) =
        input |> Decision.map (fun tree ->
            let node = tree.Value

            let coal = 
                node |> Node.message
                     |> Option.map ((=) msg)
                     |> Option.defaultValue false
                     |> (&&) (not (prov.persistForStory node))
                     |> (&&) (ZTree.isLeaf tree)                

            match coal with
                | true ->
                    let v = { node with state = state }
                    Decided (tree |> ZTree.set v)
                | false ->
                    Undecided tree
        )

    let private coalesceWithChild (prov : Provenance) (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>) =
        input |> Decision.map (fun tree ->
            let c =
                tree |> ZTree.filterChildren (fun n ->
                            (n.message = Some msg) && not (prov.persistForStory n)
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

    let private appendNew (state : Reduced.State) (msg : Reduced.Message) (input : Decision<Node ztree>)=
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

        let state = Reduced.State.create succ
        let msg = Reduced.Message.create act

        let t =
            Undecided prov.tree
                |> checkMessage msg
                |> checkStateChanged state
                |> checkParent state
                |> checkChildren state msg
                |> coalesceWithCurrent prov state msg
                |> coalesceWithChild prov state msg
                |> appendNew state msg
                |> Decision.get

        { prov with tree = t }

    let restore (prov : Provenance) (model : AppModel) =
        Reduced.State.restore model prov.tree.Value.state

    let init (model : AppModel) =
        { tree = Node.create (Reduced.State.create model) None
                    |> ZTree.single 
          persistForStory = fun _ -> false }

    let updateNode (f : Node -> Node) (prov : Provenance) =
        { prov with tree = prov.tree |> ZTree.update f}

    let setPersistForStory (f : Node -> bool) (prov : Provenance) =
        { prov with persistForStory = f }
