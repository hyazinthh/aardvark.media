module StoryApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Animation

open Story
open Model
open Provenance
open Annotations

[<AutoOpen>]
module private Helpers =

    module Annotations =
        // Updates the annotations
        let update (msg : AnnotationAction) (model : Model) =
            let sel = Option.get model.story.selected

            let content =
                match sel.content with
                    | FrameContent (n, p, a) -> 
                        FrameContent (n, p, a |> AnnotationApp.update msg)               
                    | x -> x

            let sel = { sel with content = content }
            model |> Lens.update Model.Lens.story (Story.set sel sel)

    // Sets the story and updates the provenance
    // accordingly
    let saveAndUpdate (f : Story -> Story) (model : Model) =

        // Updates the currently selected slide (if not None)
        // with changes from model
        let saveChanges (model : Model) =

            let updateSelected (sel : Slide) =
                match sel.content with
                    | TextContent _ -> sel
                    | FrameContent (_, _, a) ->
                        let n = Provenance.current model.provenance
                        let p = Model.getPresentation model
                        sel |> Lens.set Slide.Lens.content (FrameContent (n, p, a))

            model |> Lens.update Model.Lens.story (fun s ->
                match s.selected with
                    | None -> s
                    | Some sel ->
                        s |> Story.saveChanges (sel |> updateSelected)
            )

        model |> if Model.isAnimating model then id else saveChanges
              |> Lens.update Model.Lens.story f

    // Restores the model from the selected slide
    let restore (animate : bool) (model : Model) =
        match model.story |> Story.selected |> Slide.content with
            | FrameContent (node, presentation, _) ->
                let p = model.provenance |> ProvenanceApp.update model.story (Goto node.id)
                let m = ProvenanceApp.restore model.appModel p

                model |> Lens.set Model.Lens.provenance p
                      |> Lens.set Model.Lens.appModel m
                      |> Model.setPresentation presentation
                      |> if animate then AnimationApp.animate 0.5 model else id
            | _ -> 
                model

    let getViewProjTrafo (model : MModel) =
        let cfg = RenderControlConfig.standard

        adaptive {
            let! s = model.renderControlSize
            let! f = model.appModel.frustum
            let! v = model.appModel.camera.view

            return f |> cfg.adjustAspect s
                     |> Camera.create v
                     |> Camera.viewProjTrafo
        }

    let getSceneHit (model : MModel) =
        model.appModel.sceneHit

 [<AutoOpen>]
 module private Events =
    // Fired when a slide is dragged and dropped
    let onSlideMove (cb : SlideId * SlideId option * SlideId option -> 'msg) =
        onEvent "onslidemove" [] (fun args ->
            let x = args |> List.toArray |> Array.map (Pickler.unpickleOfJson >> SlideId.tryParse)
            cb (x.[0].Value, x.[1], x.[2])
        )

let init = {
    slides = PList.empty
    selected = None
    showAnnotations = false
    thumbnailRequests = HSet.empty
}

let update (msg : StoryAction) (model : Model) =
    match msg with
        | AnnotationAction a ->
            model |> Annotations.update a

        | Forward when (Story.isActive model.story) ->
            model |> saveAndUpdate Story.forward
                  |> restore true

        | Backward when (Story.isActive model.story) ->
            model |> saveAndUpdate Story.backward
                  |> restore true

        | SelectSlide id -> 
            model |> saveAndUpdate (Story.selectById (Some id))
                  |> restore true

        | RemoveSlide id ->
            model |> saveAndUpdate (Story.removeById id)

        | MoveSlide (id, l, r) ->
            let story = 
                if l.IsSome then
                    Story.moveAfterById id l.Value
                else
                    Story.moveBeforeById id r.Value

            model |> saveAndUpdate story

        | AddFrameSlide before ->
            let slide = Slide.frame model.provenance (Model.getPresentation model) Thumbnail.empty
            let story = 
                match before with
                    | None -> Story.append slide
                    | Some id -> Story.insertBeforeById slide id

            model |> saveAndUpdate story
                  |> ThumbnailApp.request slide.id

        | AddTextSlide _ ->
            model

        | DeselectSlide ->
            model |> saveAndUpdate (Story.select None)

        | ThumbnailUpdated (id, t) ->
            let slide = model.story |> Story.tryFindById id

            match slide with
                | Some s -> 
                    model |> saveAndUpdate (Story.set s { s with thumbnail = t })
                | None -> 
                    model
            |> ThumbnailApp.removeRequest id

        | ToggleAnnotations ->
            model |> Lens.update (Model.Lens.story |. Story.Lens.showAnnotations) not

        | _ -> model

let threads (model : Model) =
    ThumbnailApp.threads model

let overlayView (model : MModel) =

    // We don't want buttons to take away the focus from
    // the textareas of the labels
    let preventBlur (x : DomNode<'a>) =
        onBoot "$('#__ID__').on('mousedown', function (event) { event.preventDefault(); });" x

    let overlay (a : MAnnotations) =
        let focus = a.focus |> Mod.map Option.isSome

        div [clazz "frame"] [
            i [clazz "huge camera icon"] []
            i [
                clazz "huge remove link icon"
                onClick (fun _ -> DeselectSlide)
            ] []

            Incremental.div (AttributeMap.ofList [clazz "annotation menu"]) <|
                alist {
                    let! hidden = model.story.showAnnotations |> Mod.map not

                    yield div [
                        clazz ("ui icon toggle button" + if hidden then "" else " active")
                        onClick (fun _ -> ToggleAnnotations)
                    ] [
                        i [clazz "comments icon"] []
                    ]

                    if not hidden then
                        yield div [ clazz "ui vertical buttons" ] [
                            preventBlur (
                                Incremental.div (AttributeMap.ofAMap <| amap {
                                    let! focus = focus
                                    yield clazz <| "ui icon button" + if focus then "" else " disabled"
                                }) <| AList.ofList [
                                    i [clazz "font icon"] []
                                ]
                            )

                            preventBlur (
                                Incremental.div (AttributeMap.ofAMap <| amap {
                                    let! focus = focus
                                    let! targeting = a.targeting

                                    yield clazz <| "ui icon button" +
                                                   (if focus then "" else " disabled") +
                                                   (if targeting then " active" else "")

                                    yield onClick (fun _ -> Target |> AnnotationAction)
                                }) <| AList.ofList [
                                    i [clazz "flag icon"] []
                                ]
                            )

                            preventBlur (
                                div [
                                    clazz "ui icon button"
                                    onClick (fun _ -> Add |> AnnotationAction)
                                ] [
                                    i [clazz "add icon"] []
                                ]
                            )
                        ]
                }
        ]

    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "overlayStyle"; url = "Overlay.css" }
    ]

    require (dependencies) (
        Incremental.div (AttributeMap.ofList [
            clazz "render overlay"
        ]) <| alist {
            let! selected = model.story.selected
            let! show = model.story.showAnnotations

            if selected.IsSome then
                let! cont = selected.Value.content

                match cont with
                    | MFrameContent (_, _, annotations) ->
                        if show then
                            let vp = getViewProjTrafo model
                            let sceneHit = getSceneHit model

                            yield annotations |> AnnotationApp.view vp sceneHit false
                                              |> UI.map AnnotationAction

                        yield overlay annotations
                    | _ -> ()
        }
    )

let storyboardView (model : MModel) =
    let dependencies = Html.semui @ [
        { kind = Stylesheet; name = "storyboardStyle"; url = "Storyboard.css" }
        { kind = Script; name = "storyboardScript"; url = "Storyboard.js" }
    ]

    let addSlideButton (before : SlideId option) =
        onBoot "initAddButton($('#__ID__'))" (
            div [clazz "add button"] [
                div [clazz "ui icon button first"] [
                    i [clazz "add icon"] []
                ]
                div [clazz "ui vertical buttons second"] [
                    div [clazz "ui button"; onClick (fun _ -> AddFrameSlide before)] [
                        i [clazz "camera icon"] []
                        text "Frame"                        
                    ]
                    div [clazz "ui button"; onClick (fun _ -> AddTextSlide before)] [
                        i [clazz "list icon"] []
                        text "Text"
                    ]
                ]
            ]
        )   

    let mkCollapsingFrame (model : MModel) (slide : MSlide) =
        // TODO: Optimize! This depends on the whole story record!
        let prev = Mod.map2 (fun story slide ->
                        story |> Story.leftOf slide |> Option.map (Slide.id >> string)
                   ) model.story.Current slide.Current

        onBoot "initCollapsingFrame($('#__ID__'))" (
            Incremental.div (AttributeMap.ofAMap <| amap {
                yield clazz "collapsing preview frame"

                let! id = slide.id
                yield attribute "right" (string id)

                let! prev = prev
                if prev.IsSome then 
                    yield attribute "left" prev.Value 
                    
            }) <| alist {
                let! id = slide.id
                yield addSlideButton (Some id)
            }
        )

    let mkStaticFrame (model : MModel) =
        // TODO: Optimize! This depends on the whole story record!
        let last = model.story.Current |> Mod.map (fun s ->
                        s |> Story.last |> Option.map (Slide.id >> string)
                   )    

        onBoot "setupDropEvents($('#__ID__'))" (
            Incremental.div (AttributeMap.ofAMap <| amap {
                yield clazz "static preview frame"

                let! last = last
                if last.IsSome then
                    yield attribute "left" last.Value

            }) <| AList.ofList [
                yield addSlideButton None
            ]
        )
        
    let mkSlide (model : MModel) (slide : MSlide)  =
        onBoot "setupDragEvents($('#__ID__'))" (
            Incremental.div (AttributeMap.ofAMap <| amap {
                let! id = slide.id

                let! selected = adaptive {
                    let! sel = model.story.selected

                    match sel with
                        | None -> return false
                        | Some x -> return! x.id |> Mod.map ((=) id)
                }

                yield clazz ("frame" + if selected then " selected" else "")
                yield attribute "slide" (string id)
                yield onClick (if selected then (fun _ -> DeselectSlide) else (fun _ -> SelectSlide id))

            }) <| AList.ofList [
                Incremental.div (AttributeMap.ofList [clazz "thumbnail"]) <| alist {
                    let updateThumb = "thumbData.onmessage = function (data) { setupThumbnail($('#__ID__'), data); };"
                    let thumbChannel = slide.thumbnail |> Mod.map string
                                                       |> Mod.channel

                    yield onBoot' ["thumbData", thumbChannel] updateThumb (
                        img []
                    )

                    let! content = slide.content

                    match content with
                        | MFrameContent (_, _, annotations) ->
                            let vp = getViewProjTrafo model
                            let sceneHit = getSceneHit model

                            yield annotations |> AnnotationApp.view vp sceneHit true
                                              |> UI.map AnnotationAction
                        | _ -> ()
                }
            
                onBoot "disableClickPropagation($('#__ID__'))" (
                    Incremental.div (AttributeMap.ofAMap <| amap {
                        let! id = slide.id
                        yield clazz "ui icon remove slide button"
                        yield onClick (fun _ -> RemoveSlide id)

                    }) <| AList.ofList [
                        i [clazz "remove icon"] []
                    ]
                )

                Incremental.div (AttributeMap.ofList [clazz "ui floating blue label"]) <| alist {
                    // TODO: Optimize! This depends on the whole story record!
                    let! s = slide.Current
                    let! index = model.story.Current |> Mod.map (Story.findIndex s)
                    yield text (string (index + 1))
                }
            ]
        )

    require dependencies (
        Incremental.div (AttributeMap.ofList [ 
            clazz "storyboard"
            onSlideMove MoveSlide 
        ]) <| alist {
            for s in model.story.slides do
                yield s |> mkCollapsingFrame model
                yield s |> mkSlide model

            yield mkStaticFrame model
        }
    )