namespace Story

open System
open Aardvark.Base
open Provenance
open Aardvark.Rendering.Text
open Aardvark.Base.Incremental

[<DomainType>]
type Text = {
    position : V2d
    font : Font
    text : string
}

type Content =
    | TextContent of List<Text>
    | FrameContent of Node * CameraView

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

    let frame (provenance : Provenance) (view : CameraView) = {
        id = newId ()
        content = FrameContent (provenance.tree.Value, view)
    }  
    
    let id (slide : Slide) =
        slide.id

    let content (slide : Slide) =
        slide.content

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Story =

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
                | FrameContent (n, _) -> n.id = node.id
        ) |> Option.isSome

    let find (slide : Slide) (s : Story) =
        s.slides |> ZList.find (fun s -> s.id = slide.id)

    let select (sel : Slide option) (s : Story) =
        { s with selected = sel }

    let forward (s : Story) =
        let r = s.selected |> Option.bind (fun x ->
                    s |> find x
                      |> ZList.right
                      |> Option.bind ZList.tryHead )

        if r.IsNone then s else { s with selected = r}

    let backward (s : Story) =
        let l = s.selected |> Option.bind (fun x ->
                    s |> find x
                      |> ZList.left
                      |> Option.bind ZList.tryHead )

        if l.IsNone then s else { s with selected = l}

    let goto (slide : Slide) (s : Story) =
        { s with selected = Some slide }

    let append (slide : Slide) (s : Story) =
        { slides = s.slides |> ZList.append slide
          selected = Some slide }

    let remove (slide : Slide) (s : Story) =
        { slides = s.slides |> ZList.remove (fun s -> s.id = slide.id)
          selected = s.selected |> Option.bind (fun s -> if s.id = slide.id then None else Some s) }