namespace Model

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.UI.Primitives

open BoxSelection
open Provenance
open Story

type Action =
    | AppAction          of AppAction
    | UpdateConfig       of DockConfig
    | NodeClick          of NodeId
    | SlideClick         of SlideId
    | RemoveSlide        of SlideId
    | AddFrameSlide      of SlideId option
    | AddTextSlide       of SlideId option
    | KeyDown            of key : Keys
    | KeyUp              of key : Keys

[<DomainType>]
type Model = {
    appModel : AppModel
    dockConfig : DockConfig
    provenance : Provenance
    story : Story
    presentation : bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Model =

    let setView (view : CameraView) (model : Model) =
        let app = model.appModel
        let cam = model.appModel.camera

        { model with 
            appModel = { app with camera = { cam with view = view } }
        }

    let getView (model : Model) =
        model.appModel.camera.view

    let setRendering (rendering  : RenderingParams) (model : Model) =
        { model with
            appModel = { model.appModel with rendering = rendering }
        }

    let getRendering (model : Model) =
        model.appModel.rendering
