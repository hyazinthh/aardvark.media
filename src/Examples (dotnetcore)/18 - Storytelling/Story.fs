namespace Story

open System
open System.Collections.Generic
open Aardvark.Base
open Aardvark.Rendering.Text
open Aardvark.Base.Incremental

open Model
open Provenance

[<DomainType>]
type Text = {
    position : V2d
    font : Font
    text : string
}

[<DomainType>]
type PresentationParams = {
    view : Reduced.CameraView;
    rendering : RenderingParams;
}

type Content =
    | TextContent of List<Text>
    | FrameContent of Node * PresentationParams

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

    let frame (provenance : Provenance) (presentation : PresentationParams) = {
        id = newId ()
        content = FrameContent (provenance.tree.Value, presentation)
    }  
    
    let id (slide : Slide) =
        slide.id

    let content (slide : Slide) =
        slide.content

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Story =

    let private findInList (slide : SlideId) (s : Story) =
        s.slides |> ZList.find (fun s -> s.id = slide)

    let empty = {
        slides = ZList.empty
        selected = None
    }

    let isActive (s : Story) =
        Option.isSome s.selected

    let trySelected (s : Story) = s.selected

    let selected (s : Story) =
        match trySelected s with
            | None -> raise (ArgumentException ("No slide selected"))
            | Some s -> s

    let persistNode (s : Story) (node : Node) =
        s.slides |> ZList.tryFind (fun x ->
            match x.content with
                | TextContent _ -> false
                | FrameContent (n, _) -> (n.id = node.id) && (s.selected <> Some x)
        ) |> Option.isSome

    let tryFindIndex (slide : Slide) (s : Story) =
        s.slides |> ZList.tryFindIndex (fun s -> s.id = slide.id)

    let findIndex (slide : Slide) (s : Story) =
        match (s |> tryFindIndex slide) with
            | None -> raise (KeyNotFoundException ())
            | Some i -> i

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

    let append (slide : Slide) (s : Story) =
        { s with slides = s.slides |> ZList.append slide
                 selected = Some slide }

    let insertBefore (slide : Slide) (before : Slide) (s : Story) =
        { s with slides = s.slides |> ZList.find (fun s -> s.id = before.id)
                                   |> ZList.insertBefore slide
                 selected = Some slide }

    let remove (slide : Slide) (s : Story) =
        { s with slides = s.slides |> ZList.remove (fun s -> s.id = slide.id)
                 selected = s.selected |> Option.bind (fun s -> if s.id = slide.id then None else Some s) }

    let set (slide : Slide) (value : Slide) (s : Story) =
        { s with slides = s.slides |> ZList.find (fun s -> s.id = slide.id)
                                   |> ZList.set value
                 selected = if s.selected = Some slide then
                                Some value
                            else
                                s.selected }

    let length (s : Story) =
        s.slides |> ZList.length

    let toList (s : Story) =
        s.slides |> ZList.toList

    let update (provenance : Provenance) (presentation : PresentationParams) (s : Story) =
        s.selected |> Option.map (fun sel ->
            match sel.content with
                | FrameContent (n, p) when (n, p) <> (provenance.tree.Value, presentation) ->
                    let x = { sel with content = FrameContent (provenance.tree.Value, presentation) }
                    s |> set sel x
                | _ -> s
        )
        |> Option.defaultValue s

    let updateProvenance (s : Story) (provenance : Provenance) =
        provenance |> Provenance.setPersistForStory (persistNode s)