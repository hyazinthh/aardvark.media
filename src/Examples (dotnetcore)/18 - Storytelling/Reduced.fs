namespace Provenance.Reduced

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Trafos

open Model
open BoxSelection

type OCameraView = CameraView

// TODO: We need to redefine this because we want structural equality but CameraView is a class :/
[<DomainType>]
type CameraView = {
    sky : V3d
    location : V3d
    forward : V3d
    up : V3d
    right : V3d
}

type OMessage = BoxSelectionAction

[<DomainType>]
type Message =
    | Scale            of BoxId
    | Rotate           of BoxId
    | Translate        of BoxId
    | AddBox
    | RemoveBox
    | Unknown 

    override x.ToString () =
        match x with
            | Scale _       -> "S"
            | Rotate _      -> "R"
            | Translate _   -> "T"
            | AddBox        -> "A"
            | RemoveBox     -> "D"
            | Unknown       -> ""

type OTransformation = Transformation

[<DomainType>]
type Transformation = {
    pose : Pose
}

type OVisibleBox = VisibleBox

[<DomainType>]
type VisibleBox = {
    geometry : Box3d
    color    : C4b    
    transform : Transformation

    [<NonIncremental; PrimaryKey>]
    id : BoxId
}

type OState = AppModel

[<DomainType; CustomEquality; NoComparison>]
type State = 
    { boxes : hmap<BoxId, VisibleBox>
      nextColor : ColorIndex } 

    override x.GetHashCode () =
        hash x.boxes
    override x.Equals y =
        match y with 
            | :? State as y -> x.boxes = y.boxes
            | _ -> false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CameraView =
    
    let create (v : OCameraView) =
        { sky = v.Sky
          location = v.Location
          forward = v.Forward
          up = v.Up
          right = v.Right }

    let restore (v : CameraView) =
        OCameraView (v.sky, v.location, v.forward, v.up, v.right)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
    
    let create : (OMessage -> Message) = function
        | AppAction.Scale (id, _) -> Scale id
        | AppAction.Rotate (id, _) -> Rotate id
        | AppAction.Translate (id, _) -> Translate id
        | AppAction.AddBox -> AddBox
        | AppAction.RemoveBox -> RemoveBox
        | _ -> Unknown

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Transformation =

    let create (t : OTransformation) =
        { pose = t.pose }
        
    let restore (current : OTransformation) (t : Transformation) =
        { current with pose = t.pose
                       workingPose = t.pose 
                       previewTrafo = Pose.toTrafo t.pose }

    let restore' (t : Transformation) = 
        t |> restore TrafoController.initial

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VisibleBox =

    let create (b : OVisibleBox) =
        { geometry = b.geometry
          color = b.color
          transform = Transformation.create b.transform
          id = b.id }

    let createMap (b : hmap<BoxId, OVisibleBox>) =
        b |> HMap.map (fun _ b -> create b)

    let restore (current : OVisibleBox) (b : VisibleBox) =
        { current with geometry = b.geometry
                       color = b.color
                       transform = Transformation.restore current.transform b.transform
                       id = b.id }

    let restore' (b : VisibleBox) : OVisibleBox =
        { geometry = b.geometry
          color = b.color
          transform = b.transform |> Transformation.restore' 
          id = b.id }

    let restoreMap (current : hmap<BoxId, OVisibleBox>) (b : hmap<BoxId, VisibleBox>) =
        HMap.choose2 (fun k l r ->
            r |> Option.map (fun r -> 
                    match l with
                        | None -> restore' r
                        | Some l -> restore l r
            )
        ) current b

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    
    let create (s : OState) =
        { boxes = s.boxes |> VisibleBox.createMap
          nextColor = s.nextColor }
        
    let restore (current : OState) (s : State) =
        { current with boxes = s.boxes |> VisibleBox.restoreMap current.boxes
                       nextColor = s.nextColor 
                       selectedBoxes = HSet.empty }

    let boxes (s : State) = s.boxes

    let nextColor (s : State) = s.nextColor