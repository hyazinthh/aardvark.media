namespace Model

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.UI.Primitives
open Aardvark.UI.Animation

open BoxSelection
open Provenance
open Story
open Annotations

type Action =
    | AppAction          of AppAction
    | AnnotationAction   of AnnotationAction
    | UpdateConfig       of DockConfig
    | NodeClick          of NodeId
    | SlideClick         of SlideId
    | RemoveSlide        of SlideId
    | EditSlide          of SlideId
    | MoveSlide          of SlideId * SlideId option * SlideId option
    | AddFrameSlide      of SlideId option
    | AddTextSlide       of SlideId option
    | DeselectSlide
    | ThumbnailUpdated   of SlideId * Thumbnail
    | ToggleAnnotations
    | AnimationAction    of AnimationAction
    | KeyDown            of key : Keys
    | KeyUp              of key : Keys

[<DomainType>]
type Animation = {
    model : AnimationModel
    savedView : CameraView
}

[<DomainType>]
type Model = {
    appModel : AppModel
    dockConfig : DockConfig
    provenance : Provenance
    story : Story
    presentation : bool
    thumbnailRequests : SlideId hset
    animation : Animation
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AppModel =

    let setView (view : CameraView) (model : AppModel) =
        { model with  camera = { model.camera with view = view } }

    let getView (model : AppModel) =
        model.camera.view |> Reduced.CameraView.create

    let setRendering (rendering  : RenderingParams) (model : AppModel) =
        { model with rendering = rendering }

    let getRendering (model : AppModel) =
        model.rendering

    let setPresentation (presentation : PresentationParams) (model : AppModel) =
        model |> setView (presentation.view |> Reduced.CameraView.restore)
              |> setRendering presentation.rendering

    let getPresentation (model : AppModel) =
        { view = model |> getView;
          rendering = model |> getRendering }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Model =

    let setView (view : CameraView) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setView view }

    let getView (model : Model) =
        model.appModel.camera.view

    let setRendering (rendering  : RenderingParams) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setRendering rendering }

    let getRendering (model : Model) =
        model.appModel.rendering

    let setPresentation (presentation : PresentationParams) (model : Model) =
        { model with appModel = model.appModel |> AppModel.setPresentation presentation }

    let getPresentation (model : Model) =
        model.appModel |> AppModel.getPresentation

    let isAnimating (model : Model) =
        model.animation.model |> AnimationApp.shouldAnimate
