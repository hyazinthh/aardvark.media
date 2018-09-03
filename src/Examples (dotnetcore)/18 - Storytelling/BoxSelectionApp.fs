module BoxSelectionApp

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open BoxSelection


let defaultBox =
    Box3d.FromCenterAndSize (V3d.Zero, V3d.One * 2.0)

let getColor = function
        | ColorIndex i -> Primitives.colors.[i % Primitives.colors.Length]

let getNextColor = function
        | ColorIndex i -> ColorIndex ((i + 1) % Primitives.colors.Length)

let mkVisibleBox (color : ColorIndex) (box : Box3d) : VisibleBox = 
    {
        id = BoxId (Guid.NewGuid().ToString())
        geometry = box
        color = getColor color
        transform = TrafoController.initial
    }

// Returns if a trafo controller is grabbed
let isGrabbed (model : BoxSelectionModel) =
    model.selectedBoxes 
        |> HSet.exists (fun id ->
             let b = HMap.find id model.boxes
             b.transform.grabbed.IsSome
        )

let isGrabbed' (model : MBoxSelectionModel) =
    adaptive {
        let! sel = model.selectedBoxes |> ASet.toMod
        let! map = model.boxes |> AMap.toMod

        return! sel 
            |> HRefSet.fold (fun r id ->
                    let b = HMap.find id map
                    b.transform.grabbed
                        |> Mod.map Option.isSome
                        |> Mod.map2 (||) r
                ) ((Mod.init false) :> IMod<bool>)
    }

let update (model : BoxSelectionModel) (act : BoxSelectionAction) =

    let transformBox update id act map =
        map |> HMap.alter id (Option.map (fun b ->
            { b with transform = update b.transform act }
        ))
        
    match act with
        | CameraMessage m when not (isGrabbed model) -> 
            { model with camera = FreeFlyController.update model.camera m }
        | RenderingAction a ->
            { model with rendering = RenderingParameters.update model.rendering a }
        | Scale (id, a) ->
            { model with boxes = model.boxes |> transformBox ScaleController.updateController id a }
        | Rotate (id, a) ->
            { model with boxes = model.boxes |> transformBox RotationController.updateController id a }
        | Translate (id, a) ->
            { model with boxes = model.boxes |> transformBox TranslateController.updateController id a }
        | SetTrafoKind kind ->
            { model with trafoKind = kind }
        | SetTrafoMode mode ->
            let l = VisibleBox.Lens.transform |. Transformation.Lens.mode

            { model with trafoMode = mode 
                         boxes = model.boxes |> HMap.map (fun _ b -> l.Set (b, mode)) }

        | Select id -> 
            let selection = 
                if HSet.contains id model.selectedBoxes 
                then HSet.remove id model.selectedBoxes 
                else HSet.add id model.selectedBoxes

            { model with selectedBoxes = selection }           
        | Enter id -> { model with boxHovered = Some id }            
        | Exit -> { model with boxHovered = None }                             
        | AddBox ->               
            let box = defaultBox |> mkVisibleBox model.nextColor
                                         
            { model with boxes = model.boxes |> HMap.add box.id box 
                         selectedBoxes = [ box.id ] |> HSet.ofList 
                         nextColor = getNextColor model.nextColor }
        | RemoveBox ->
            { model with boxes = model.selectedBoxes |> HSet.fold (fun m i -> HMap.remove i m) model.boxes
                         selectedBoxes = HSet.empty }

        | ClearSelection -> { model with selectedBoxes = HSet.empty}
        | _ -> model
                        
let myCss = { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }

let mkColor (model : MBoxSelectionModel) (box : MVisibleBox) =
    let id = box.id 

    let color =  
        model.selectedBoxes 
            |> ASet.contains id 
            |> Mod.bind (function 
                | true -> Mod.constant Primitives.selectionColor 
                | false -> box.color
              )

    model.boxHovered 
        |> Mod.bind (function 
            | Some k when k = id -> Mod.constant Primitives.hoverColor
            | _ -> color
         )

let mkISg (model : MBoxSelectionModel) (box : MVisibleBox) =
                
    let color = mkColor model box

    adaptive {
        let! grabbed = isGrabbed' model
        let events = 
            if not grabbed then [
                Sg.onClick (fun _ -> Select box.id)
                Sg.onEnter (fun _ -> Enter box.id)
                Sg.onLeave (fun () -> Exit)
            ] else List.empty

        return Sg.box color box.geometry
            |> Sg.trafo box.transform.previewTrafo
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.simpleLighting
                }                
            |> Sg.requirePicking
            |> Sg.noEvents
            |> Sg.withEvents events
    } |> Sg.dynamic

let renderView (model : MBoxSelectionModel) =

    let frustum =
        Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

    let controllers =
        alist {
            let! kind = model.trafoKind

            let (viewController, msg) =
                match kind with
                    | TrafoKind.Scale -> (ScaleController.viewController, Scale)
                    | TrafoKind.Rotate -> (RotationController.viewController, Rotate)
                    | _ -> (TranslateController.viewController, Translate)

            for id in (model.selectedBoxes |> ASet.toAList) do
                let sg = model.boxes |> AMap.find id |> Mod.map (fun b ->
                        b.transform |> viewController (fun a -> msg (id, a)) model.camera.view
                    )

                yield sg |> Sg.dynamic
        } 
        |> AList.toASet |> Sg.set

    let scene = 
        model.boxes 
            |> AMap.toASet 
            |> ASet.map (function (_, b) -> mkISg model b)
            |> Sg.set
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo
                toEffect DefaultSurfaces.vertexColor
                toEffect DefaultSurfaces.simpleLighting                              
                ]
            |> Sg.fillMode model.rendering.fillMode
            |> Sg.cullMode model.rendering.cullMode
            |> Sg.andAlso controllers        

    FreeFlyController.controlledControl model.camera CameraMessage frustum
        (AttributeMap.ofList [
            style "width:100%; height: 100%"
        ]) scene

let renderingControlsView (model : MBoxSelectionModel) = 
    RenderingParameters.view model.rendering |> UI.map RenderingAction 

let controlsView (model : MBoxSelectionModel) =
    require (Html.semui) (
        div [clazz "ui"; style "overflow:auto; background: #1B1C1E"] [
            Incremental.div AttributeMap.Empty (
                alist {
                    let! empty = model.selectedBoxes
                                    |> ASet.toMod
                                    |> Mod.map HRefSet.isEmpty

                    if not empty then
                        yield Html.SemUi.accordion "Transform" "cogs" true [
                            Html.table [           
                                Html.row "Type:" [Html.SemUi.dropDown model.trafoKind SetTrafoKind]      
                                Html.row "Mode:" [Html.SemUi.dropDown model.trafoMode SetTrafoMode]
                            ]
                        ]  
                }
            )

            Html.SemUi.accordion "Boxes" "cubes" true [
                Incremental.div
                    (AttributeMap.ofList [clazz "ui buttons"]) (
                        alist {
                            let! empty = model.selectedBoxes 
                                            |> ASet.toMod
                                            |> Mod.map HRefSet.isEmpty

                            yield button [clazz "ui button"; onMouseClick (fun _ -> AddBox)] [text "Add"]

                            if not empty then
                                yield button [clazz "ui button"; onMouseClick (fun _ -> RemoveBox)] [text "Remove"]
                                yield button [clazz "ui button"; onMouseClick (fun _ -> ClearSelection)] [text "Clear"]
                        }
                )

                Incremental.div 
                    (AttributeMap.ofList [clazz "ui divided list"]) (
                        alist {                                
                            for (_, b) in (model.boxes |> AMap.toASet |> ASet.toAList) do
                                let! c = mkColor model b

                                let bgc = sprintf "background: %s" (Html.ofC4b c)
                                
                                yield 
                                    div [
                                        clazz "item"; style bgc; 
                                        onClick(fun _ -> Select b.id)
                                        onMouseEnter(fun _ -> Enter b.id)
                                        onMouseLeave(fun _ -> Exit)
                                     ] [
                                        i [clazz "file outline middle aligned icon"][]
                                     ]                                                                    
                        }     
                )
            ]
        ]
    )        
          
let initial =
    {
        camera           = FreeFlyController.initial            
        rendering        = RenderingParametersModel.RenderingParameters.initial            
        boxHovered       = None
        boxes            = defaultBox
                             |> List.replicate 3
                             |> List.mapi (fun i b ->
                                    let b = mkVisibleBox (ColorIndex i) b
                                    let p = Pose.translate (V3d.IOO * 3.0 * float i)
                                  
                                    (b.id, VisibleBox.Lens.transform.Update(b, fun t ->
                                                { t with pose = p 
                                                         previewTrafo = Pose.trafo p }
                                            )
                                    )
                                )
                             |> HMap.ofList

        selectedBoxes    = HSet.empty         
        trafoKind        = TrafoKind.Translate
        trafoMode        = TrafoController.initial.mode
        nextColor        = ColorIndex 3
    }

let app : App<BoxSelectionModel,MBoxSelectionModel,BoxSelectionAction> =
    {
        unpersist = Unpersist.instance
        threads = fun model -> FreeFlyController.threads model.camera |> ThreadPool.map CameraMessage
        initial = initial
        update = update
        view = fun _ -> body [] []
    }

