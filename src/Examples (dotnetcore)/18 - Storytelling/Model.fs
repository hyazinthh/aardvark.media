namespace Model

open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.UI.Primitives

open BoxSelection
open Provenance
open Story

type Action =
    | BoxSelectionAction of BoxSelectionAction
    | UpdateConfig       of DockConfig
    | NodeClick          of NodeId
    | SlideClick         of Slide
    | KeyDown            of key : Keys
    | KeyUp              of key : Keys

[<DomainType>]
type Model = {
    appModel : BoxSelectionModel
    dockConfig : DockConfig
    provenance : Provenance
    story : Story
    presentation : bool
}