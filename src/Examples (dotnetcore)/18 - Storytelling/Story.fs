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

// Record containing parameters that influence
// the rendering of frame slides
[<DomainType>]
type PresentationParams = {
    view : Reduced.CameraView;
    rendering : RenderingParams;
}

// A slide can either contain raw text
// or a frame referencing an analysis state from the provenance graph
type Content =
    | TextContent of List<Text>
    | FrameContent of Node * PresentationParams

    static member isText = function
        | TextContent _ -> true
        | _ -> false

    static member isFrame = function
        | FrameContent _ -> true
        | _ -> false

// Each slide has an id
type SlideId = 
    private SlideId of Guid with

    static member generate () =
        SlideId (Guid.NewGuid ())

    static member ofGuid (v : Guid) =
        SlideId v

    static member parse (s : string) =
        s |> Guid.Parse |> SlideId

    static member tryParse (s : string) =
        try
            Some (s |> Guid.Parse |> SlideId)
        with
            | _ -> None

    override x.ToString () =
        let (SlideId v) = x in string v

// Each slide has a preview thumbnail which
// is saved as a byte array and passed to Javascript as a base64 string
type Thumbnail =
    private Thumbnail of byte [] with

    static member create (data : byte []) =
        Thumbnail data

    static member empty =
        Array.empty |> Thumbnail.create

    override x.ToString () =
        let (Thumbnail d) = x in d |> Convert.ToBase64String

// The slide record
[<DomainType>]
type Slide =
    { id : SlideId
      content : Content 
      thumbnail : Thumbnail }

 // A story is a list of slides and optionally
 // a currently selected slide
[<DomainType>]
type Story = {
    slides : Slide plist
    selected : Slide option
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Slide =

    let frame (provenance : Provenance) (presentation : PresentationParams) (thumbnail : Thumbnail) =
        { id = SlideId.generate ()
          content = FrameContent (provenance.tree.Value, presentation)
          thumbnail = thumbnail }

    let id (slide : Slide) = slide.id

    let content (slide : Slide) = slide.content

    let isText (slide : Slide) =
        slide |> content |> Content.isText

    let isFrame (slide : Slide) =
        slide |> content |> Content.isFrame

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Story =

    [<AutoOpen>]
    module private Helpers =

        let findInList (slide : Slide) (s : Story) =
            s.slides |> PList.findIndex (fun s -> s.id = slide.id)

    let empty = {
        slides = PList.empty
        selected = None
    }

    let isActive (s : Story) =
        Option.isSome s.selected

    let trySelected (s : Story) = s.selected

    let selected (s : Story) =
        match trySelected s with
            | None -> raise (ArgumentException ("No slide selected"))
            | Some s -> s

    let tryFindIndex (slide : Slide) (s : Story) =
        s.slides |> PList.tryFindIndex' (fun s -> s.id = slide.id)

    let findIndex (slide : Slide) (s : Story) =
        match (s |> tryFindIndex slide) with
            | None -> raise (KeyNotFoundException ())
            | Some i -> i

    let tryFind (predicate : Slide -> bool) (s : Story) =
        s.slides |> PList.tryFind predicate

    let find (predicate : Slide -> bool) (s : Story) =
        match (s |> tryFind predicate) with
            | None -> raise (KeyNotFoundException ())
            | Some s -> s

    let tryFindById (id : SlideId) (s : Story) =
        s |> tryFind (fun s -> s.id = id)

    let findById (id : SlideId) (s : Story) =
        s |> find (fun s -> s.id = id)

    let contains (slide : Slide) (s : Story) =
        s |> tryFindById slide.id |> Option.isSome

    let rightOf (slide : Slide) (s : Story) =
        s.slides |> PList.right (s |> findInList slide)

    let leftOf (slide : Slide) (s : Story) =
        s.slides |> PList.left (s |> findInList slide)

    let first (s : Story) =
        s.slides |> PList.tryHead

    let last (s : Story) =
        s.slides |> PList.tryLast

    let select (sel : Slide option) (s : Story) =
        { s with selected = sel }

    let selectById (id : SlideId option) (s : Story) =
        s |> select (id |> Option.map (fun id -> s |> findById id))

    let forward (s : Story) =
        let r = s.selected |> Option.bind (fun x ->
                    s |> rightOf x
                )

        if r.IsNone then s else { s with selected = r}

    let backward (s : Story) =
        let l = s.selected |> Option.bind (fun x ->
                    s |> leftOf x
                )

        if l.IsNone then s else { s with selected = l}

    let append (slide : Slide) (s : Story) =
        { s with slides = s.slides |> PList.append slide
                 selected = Some slide }

    let insertBefore (slide : Slide) (before : Slide) (s : Story) =
        { s with slides = s.slides |> PList.insertBefore (s |> findInList before) slide
                 selected = Some slide }

    let insertBeforeById (slide : Slide) (before : SlideId) (s : Story) =
        s |> insertBefore slide (s |> findById before)

    let insertAfter (slide : Slide) (after : Slide) (s : Story) =
        { s with slides = s.slides |> PList.insertAfter (s |> findInList after) slide
                 selected = Some slide }

    let insertAfterById (slide : Slide) (after : SlideId) (s : Story) =
        s |> insertAfter slide (s |> findById after)

    let remove (slide : Slide) (s : Story) =
        { s with slides = s.slides |> PList.remove (s |> findInList slide)
                 selected = s.selected |> Option.bind (fun s -> if s.id = slide.id then None else Some s) }

    let removeById (id : SlideId) (s : Story) =
        s |> remove (s |> findById id)

    let set (slide : Slide) (value : Slide) (s : Story) =
        { s with slides = s.slides |> PList.set (s |> findInList slide) value
                 selected = s.selected |> Option.map (fun s ->
                                if s.id = slide.id then value else s
                            ) }

    let moveBefore (slide : Slide) (before : Slide) (s : Story) =
        { s with slides = s.slides |> PList.remove (s |> findInList slide)
                                   |> PList.insertBefore (s |> findInList before) slide }

    let moveBeforeById (slide : SlideId) (before : SlideId) (s : Story) =
        s |> moveBefore (s |> findById slide) (s |> findById before)

    let moveAfter (slide : Slide) (after : Slide) (s : Story) =
        { s with slides = s.slides |> PList.remove (s |> findInList slide)
                                   |> PList.insertAfter (s |> findInList after) slide }

    let moveAfterById (slide : SlideId) (after : SlideId) (s : Story) =
        s |> moveAfter (s |> findById slide) (s |> findById after)

    let length (s : Story) =
        s.slides |> PList.count

    let saveChanges (modified : Slide) (s : Story) =
        { s with slides = s.slides |> PList.set (s |> findInList s.selected.Value) modified 
                 selected = Some modified }

    module Provenance =

        let persistNode (s : Story) (node : Node) =
            s.slides |> PList.tryFind (fun x ->
                match x.content with
                    | TextContent _ -> false
                    | FrameContent (n, _) -> (n.id = node.id) && (s.selected <> Some x)
            ) |> Option.isSome

        let update (s : Story) (provenance : Provenance) =
            provenance |> Provenance.setPersistForStory (persistNode s)