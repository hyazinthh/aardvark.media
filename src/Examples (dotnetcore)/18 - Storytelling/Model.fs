namespace Model

open Aardvark.Base.Incremental
open Aardvark.Application

open BoxSelection
open Provenance
open Story

type Action =
    | BoxSelectionAction of BoxSelectionAction
    | NodeClick          of NodeId
    | SlideClick         of tree<Node> option
    | KeyDown            of key : Keys
    | KeyUp              of key : Keys

[<DomainType>]
type Model = {
    appModel : BoxSelectionModel
    provenance : Provenance
    story : Story
}