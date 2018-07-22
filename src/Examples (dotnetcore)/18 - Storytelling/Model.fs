namespace Model

open Aardvark.Base.Incremental
open Aardvark.Application

open BoxSelection
open Provenance

type Action =
    | BoxSelectionAction of BoxSelectionAction
    | NodeClick          of NodeId
    | KeyDown            of key : Keys
    | KeyUp              of key : Keys

[<DomainType>]
type Model = {
    appModel : BoxSelectionModel
    provenance : Provenance
}