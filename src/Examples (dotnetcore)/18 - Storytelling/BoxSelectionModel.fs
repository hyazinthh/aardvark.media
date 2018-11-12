namespace BoxSelection

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos

open RenderingParametersModel

type BoxId = BoxId of string

type ColorIndex = ColorIndex of int

type BoxSelectionAction =
    | CameraMessage    of FreeFlyController.Message   
    | RenderingAction  of Action
    | SetTrafoKind     of TrafoKind
    | SetTrafoMode     of TrafoMode
    | Scale            of BoxId * TrafoController.Action
    | Rotate           of BoxId * TrafoController.Action
    | Translate        of BoxId * TrafoController.Action
    | Select           of BoxId    
    | Enter            of BoxId
    | Exit  
    | AddBox
    | RemoveBox
    | ClearSelection
    | MouseMoved       of V3d

[<DomainType>]
type VisibleBox = {
    geometry : Box3d
    color    : C4b    
    transform : Transformation

    [<NonIncremental; PrimaryKey>]
    id : BoxId
}

[<DomainType>]
type BoxSelectionModel = {
    camera : CameraControllerState
    frustum : Frustum
    rendering : RenderingParameters

    boxes : hmap<BoxId,VisibleBox>
    boxHovered : option<BoxId>
    selectedBoxes : hset<BoxId>

    trafoKind : TrafoKind
    trafoMode : TrafoMode

    nextColor : ColorIndex

    sceneHit : V3d
}