module AnnotationApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Annotations

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

let init = {
    list = PList.empty
    focus = None
}

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

let view (disabled : bool) (annotations : MAnnotations) =
    
    let init (content : DomNode<'a>) =
        onBoot (sprintf "baseWidth=%d; baseHeight=%d;" baseSize.X baseSize.Y) content

    let mkAnnotation (a : MAnnotation) =
        let setData = "textData.onmessage = function (data) { setText($('#__ID__'), data); };" +
                      "widthData.onmessage = function (data) { setWidth($('#__ID__'), data); };" +
                      "positionData.onmessage = function (data) { setPosition($('#__ID__'), data); };"

        onBoot (if disabled then "" else "initLabel($('#__ID__'))") (
            onBoot' [ "textData", a.label.text |> Mod.channel
                      "widthData", a.label.width |> Mod.channel
                      "positionData", a.label.position |> Mod.channel ] setData (            
                Incremental.div (AttributeMap.ofAMap <| amap {
                    yield clazz "label"

                    let! id = a.id
                    yield onLabelMoved (fun p -> (id, p) |> LabelMoved)
                    yield onLabelResized (fun w -> (id, w) |> LabelResized)

                    if disabled then
                        yield attribute "disabled" ""

                }) <| alist {
                    let! id = a.id

                    if not disabled then
                        yield div [clazz "grabber"] []

                        yield div [clazz "ui icon move button"] [
                            i [clazz "move icon"] []
                        ]

                        yield div [
                            clazz "ui icon remove button"
                            onClick (fun _ -> id |> Remove)
                        ] [
                            i [clazz "remove icon"] []
                        ]

                    yield textarea [
                        yield attribute "rows" "1"
                        yield attribute "placeholder" "Add text here..."
                        yield onChange (fun s -> LabelChanged (id, s))
                        yield onFocus (fun _ -> Focus id)
                        yield onBlur (fun _ -> Blur)

                        if disabled then
                            yield attribute "disabled" ""
                    ] []
                }
            )
        )

    let dependencies = [
        { kind = Stylesheet; name = "annotationsStyle"; url = "Annotations.css" }
        { kind = Script; name = "annotationsScript"; url = "Annotations.js" }
    ]

    require dependencies (
        init (
            annotations.list |> AList.map mkAnnotation
                             |> Incremental.div (AttributeMap.ofList [
                                clazz "annotations"
                             ])
        )
    )