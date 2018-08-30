namespace Model

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
    | SlideClick         of Slide
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

    let setView view model =
        let app = model.appModel
        let cam = model.appModel.camera

        { model with 
            appModel = { app with camera = { cam with view = view } }
        }

    let getView model =
        model.appModel.camera.view
