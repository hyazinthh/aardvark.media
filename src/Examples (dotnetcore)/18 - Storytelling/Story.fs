namespace Story

open System
open Aardvark.Base
open Provenance
open Aardvark.Rendering.Text
open Aardvark.Base.Incremental

open Model

[<DomainType>]
type Text = {
    position : V2d
    font : Font
    text : string
}

type Content =
    | TextContent of List<Text>
    | FrameContent of Node * CameraView * RenderingParams

type SlideId = SlideId of string

[<DomainType>]
type Slide = {
    id : SlideId
    content : Content
}

[<DomainType>]
type Story = {
    slides : ZList<Slide>
    selected : Slide option
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Slide =
    
    let private newId () = SlideId (Guid.NewGuid().ToString()) 

    let frame (provenance : Provenance) (view : CameraView) (rendering : RenderingParams) = {
        id = newId ()
        content = FrameContent (provenance.tree.Value, view, rendering)
    }  
    
    let id (slide : Slide) =
        slide.id

    let content (slide : Slide) =
        slide.content

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Story =

    let private findInList (slide : SlideId) (s : Story) =
        s.slides |> ZList.find (fun s -> s.id = slide)

    let isActive (s : Story) =
        Option.isSome s.selected

    let trySelected (s : Story) = s.selected

    let selected (s : Story) =
        match trySelected s with
            | None -> failwith "No slide selected"
            | Some s -> s

    let hasFrames (s : Story) (node : Node) =
        s.slides |> ZList.tryFind (fun s -> 
            match s.content with
                | TextContent _ -> false
                | FrameContent (n, _, _) -> n.id = node.id
        ) |> Option.isSome

    let tryFind (id : SlideId) (s : Story) =
        s.slides |> ZList.tryFind (fun s -> s.id = id)
                 |> Option.bind ZList.tryHead

    let find (id : SlideId) (s : Story) =
        match s |> tryFind id with
            | None -> failwith "slide not found"
            | Some s -> s

    let select (sel : Slide option) (s : Story) =
        { s with selected = sel }

    let forward (s : Story) =
        let r = s.selected |> Option.bind (fun x ->
                    s |> findInList x.id
                      |> ZList.right
                      |> Option.bind ZList.tryHead )

        if r.IsNone then s else { s with selected = r}

    let backward (s : Story) =
        let l = s.selected |> Option.bind (fun x ->
                    s |> findInList x.id
                      |> ZList.left
                      |> Option.bind ZList.tryHead )

        if l.IsNone then s else { s with selected = l}

    let goto (slide : Slide) (s : Story) =
        { s with selected = Some slide }

    let goto' (id : SlideId) (s : Story) =
        s |> goto (s |> find id)

    let append (slide : Slide) (s : Story) =
        { s with slides = s.slides |> ZList.append slide
                 selected = Some slide }

    let insertBefore' (slide : Slide) (before : SlideId) (s : Story) =
        { s with slides = s.slides |> ZList.find (fun s -> s.id = before) 
                                   |> ZList.insertBefore slide
                 selected = Some slide }

    let insertBefore (slide : Slide) (before : Slide) (s : Story) =
        s |> insertBefore' slide before.id

    let remove (slide : Slide) (s : Story) =
        { s with slides = s.slides |> ZList.remove (fun s -> s.id = slide.id)
                 selected = s.selected |> Option.bind (fun s -> if s.id = slide.id then None else Some s) }

    let remove' (id : SlideId) (s : Story) =
        s |> remove (s |> find id)
        