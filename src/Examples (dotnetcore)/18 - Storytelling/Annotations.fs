namespace Annotations

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

// Each annotation has an id
type AnnotationId = 
    private AnnotationId of Guid with

    static member generate () =
        AnnotationId (Guid.NewGuid ())

    static member ofGuid (v : Guid) =
        AnnotationId v

    static member parse (s : string) =
        s |> Guid.Parse |> AnnotationId

    static member tryParse (s : string) =
        try
            Some (s |> Guid.Parse |> AnnotationId)
        with
            | _ -> None

    override x.ToString () =
        let (AnnotationId v) = x in string v

// A label displays text at a given position
[<DomainType>]
type Label = {
    width : int
    position : V2i
    text : string
}

// An annotation is a label and optionally a target
// in world space that is to be annotated
[<DomainType>]
type Annotation = {
    id : AnnotationId
    target : V3d option
    label : Label
}

[<DomainType>]
type Annotations = {
    list : Annotation plist
    focus : Annotation option
    show : bool
}

type AnnotationAction =
    | LabelChanged  of AnnotationId * string
    | LabelMoved    of AnnotationId * V2i
    | LabelResized  of AnnotationId * int
    | Focus         of AnnotationId
    | Blur
    | Add
    | Remove        of AnnotationId
    | Toggle

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Label =
    let create (position : V2i) = {
        width = 300;
        position = position
        text = String.Empty
    }

[<AutoOpen>]
 module Events =
    // Fired after a label is resized
    let onLabelResized (cb : int -> 'msg) =
        onEvent "onlabelresized" [] (List.head >> Pickler.unpickleOfJson >> cb)

    // Fired after a label is moved
    let onLabelMoved (cb : V2i -> 'msg) =
        onEvent "onlabelmoved" [] (List.head >> Pickler.unpickleOfJson >> cb)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
    let create (target : V3d option) (label : Label) = {
        id = AnnotationId.generate ()
        target = target
        label = label
    }

    let setLabelText (text : string) (a : Annotation) =
        { a with label = { a.label with text = text }}

    let setLabelPosition (pos : V2i) (a : Annotation) =
        { a with label = { a.label with position = pos }}

    let setLabelWidth (width : int) (a : Annotation) =
        { a with label = { a.label with width = width }}

module Annotations =

    [<AutoOpen>]
    module private Helpers =

        let baseSize = V2i (800, 600)

        let rnd = new RandomSystem ()

        let findById (id : AnnotationId) (a : Annotations) =
            let index = a.list |> PList.findIndex (fun x -> x.id = id)
            (index, PList.get index a.list)

        let updateById (f : Annotation -> Annotation) (id : AnnotationId) (a : Annotations) =
            let (index, x) = findById id a
            { a with list = a.list |> PList.set index (f x) }

        let deleteById (id : AnnotationId) (a : Annotations) =
            let (index, _) = findById id a
            { a with list = a.list |> PList.remove index }

    let empty = {
        list = PList.empty
        focus = None
        show = true
    }

    let init (content : DomNode<'a>) =
        onBoot (sprintf "baseWidth=%d; baseHeight=%d;" baseSize.X baseSize.Y) content

    let update (msg : AnnotationAction) (a : Annotations) =
        match msg with
            | LabelChanged (id, text) ->
                a |> updateById (Annotation.setLabelText text) id
            | LabelMoved (id, pos) ->
                a |> updateById (Annotation.setLabelPosition pos) id
            | LabelResized (id, w) ->
                a |> updateById (Annotation.setLabelWidth w) id
            | Focus id ->
                { a with focus = a |> findById id |> snd |> Some }
            | Blur ->
                { a with focus = None }
            | Add ->
                let r = rnd.UniformV2d (Box2d (0.25, 0.25, 0.75, 0.75))
                let pos = V2i (r * V2d (baseSize))
                let x = pos |> Label.create |> Annotation.create None 

                { a with list = a.list |> PList.append x }
            | Remove id ->
                a |> deleteById id
            | Toggle ->
                { a with show = not a.show }