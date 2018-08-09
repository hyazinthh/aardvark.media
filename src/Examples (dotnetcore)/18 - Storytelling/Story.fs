namespace Story

open Aardvark.Base
open Provenance
open Aardvark.Rendering.Text
open Aardvark.Base.Incremental

[<DomainType>]
type Text = {
    position : V2d
    font : Font
    text : string
}

type Slide =
    | TextSlide of List<Text>
    | FrameSlide of tree<Node> * List<Text>

[<DomainType>]
type Story = {
    slides : plist<Slide>
    current : Index
}