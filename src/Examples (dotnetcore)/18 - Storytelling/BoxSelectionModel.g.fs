namespace BoxSelection

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open BoxSelection

[<AutoOpen>]
module Mutable =

    
    
    type MVisibleBox(__initial : BoxSelection.VisibleBox) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<BoxSelection.VisibleBox> = Aardvark.Base.Incremental.EqModRef<BoxSelection.VisibleBox>(__initial) :> Aardvark.Base.Incremental.IModRef<BoxSelection.VisibleBox>
        let _geometry = ResetMod.Create(__initial.geometry)
        let _color = ResetMod.Create(__initial.color)
        let _transform = Aardvark.UI.Trafos.Mutable.MTransformation.Create(__initial.transform)
        
        member x.geometry = _geometry :> IMod<_>
        member x.color = _color :> IMod<_>
        member x.transform = _transform
        member x.id = __current.Value.id
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : BoxSelection.VisibleBox) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_geometry,v.geometry)
                ResetMod.Update(_color,v.color)
                Aardvark.UI.Trafos.Mutable.MTransformation.Update(_transform, v.transform)
                
        
        static member Create(__initial : BoxSelection.VisibleBox) : MVisibleBox = MVisibleBox(__initial)
        static member Update(m : MVisibleBox, v : BoxSelection.VisibleBox) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<BoxSelection.VisibleBox> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module VisibleBox =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let geometry =
                { new Lens<BoxSelection.VisibleBox, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let color =
                { new Lens<BoxSelection.VisibleBox, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let transform =
                { new Lens<BoxSelection.VisibleBox, Aardvark.UI.Trafos.Transformation>() with
                    override x.Get(r) = r.transform
                    override x.Set(r,v) = { r with transform = v }
                    override x.Update(r,f) = { r with transform = f r.transform }
                }
            let id =
                { new Lens<BoxSelection.VisibleBox, BoxSelection.BoxId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
    
    
    type MBoxSelectionModel(__initial : BoxSelection.BoxSelectionModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<BoxSelection.BoxSelectionModel> = Aardvark.Base.Incremental.EqModRef<BoxSelection.BoxSelectionModel>(__initial) :> Aardvark.Base.Incremental.IModRef<BoxSelection.BoxSelectionModel>
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _rendering = RenderingParametersModel.Mutable.MRenderingParameters.Create(__initial.rendering)
        let _boxes = MMap.Create(__initial.boxes, (fun v -> MVisibleBox.Create(v)), (fun (m,v) -> MVisibleBox.Update(m, v)), (fun v -> v))
        let _boxHovered = MOption.Create(__initial.boxHovered)
        let _selectedBoxes = MSet.Create(__initial.selectedBoxes)
        let _trafoKind = ResetMod.Create(__initial.trafoKind)
        let _trafoMode = ResetMod.Create(__initial.trafoMode)
        let _nextColor = ResetMod.Create(__initial.nextColor)
        
        member x.camera = _camera
        member x.rendering = _rendering
        member x.boxes = _boxes :> amap<_,_>
        member x.boxHovered = _boxHovered :> IMod<_>
        member x.selectedBoxes = _selectedBoxes :> aset<_>
        member x.trafoKind = _trafoKind :> IMod<_>
        member x.trafoMode = _trafoMode :> IMod<_>
        member x.nextColor = _nextColor :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : BoxSelection.BoxSelectionModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                RenderingParametersModel.Mutable.MRenderingParameters.Update(_rendering, v.rendering)
                MMap.Update(_boxes, v.boxes)
                MOption.Update(_boxHovered, v.boxHovered)
                MSet.Update(_selectedBoxes, v.selectedBoxes)
                ResetMod.Update(_trafoKind,v.trafoKind)
                ResetMod.Update(_trafoMode,v.trafoMode)
                ResetMod.Update(_nextColor,v.nextColor)
                
        
        static member Create(__initial : BoxSelection.BoxSelectionModel) : MBoxSelectionModel = MBoxSelectionModel(__initial)
        static member Update(m : MBoxSelectionModel, v : BoxSelection.BoxSelectionModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<BoxSelection.BoxSelectionModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BoxSelectionModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let camera =
                { new Lens<BoxSelection.BoxSelectionModel, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
            let rendering =
                { new Lens<BoxSelection.BoxSelectionModel, RenderingParametersModel.RenderingParameters>() with
                    override x.Get(r) = r.rendering
                    override x.Set(r,v) = { r with rendering = v }
                    override x.Update(r,f) = { r with rendering = f r.rendering }
                }
            let boxes =
                { new Lens<BoxSelection.BoxSelectionModel, Aardvark.Base.hmap<BoxSelection.BoxId,BoxSelection.VisibleBox>>() with
                    override x.Get(r) = r.boxes
                    override x.Set(r,v) = { r with boxes = v }
                    override x.Update(r,f) = { r with boxes = f r.boxes }
                }
            let boxHovered =
                { new Lens<BoxSelection.BoxSelectionModel, Microsoft.FSharp.Core.Option<BoxSelection.BoxId>>() with
                    override x.Get(r) = r.boxHovered
                    override x.Set(r,v) = { r with boxHovered = v }
                    override x.Update(r,f) = { r with boxHovered = f r.boxHovered }
                }
            let selectedBoxes =
                { new Lens<BoxSelection.BoxSelectionModel, Aardvark.Base.hset<BoxSelection.BoxId>>() with
                    override x.Get(r) = r.selectedBoxes
                    override x.Set(r,v) = { r with selectedBoxes = v }
                    override x.Update(r,f) = { r with selectedBoxes = f r.selectedBoxes }
                }
            let trafoKind =
                { new Lens<BoxSelection.BoxSelectionModel, Aardvark.UI.Trafos.TrafoKind>() with
                    override x.Get(r) = r.trafoKind
                    override x.Set(r,v) = { r with trafoKind = v }
                    override x.Update(r,f) = { r with trafoKind = f r.trafoKind }
                }
            let trafoMode =
                { new Lens<BoxSelection.BoxSelectionModel, Aardvark.UI.Trafos.TrafoMode>() with
                    override x.Get(r) = r.trafoMode
                    override x.Set(r,v) = { r with trafoMode = v }
                    override x.Update(r,f) = { r with trafoMode = f r.trafoMode }
                }
            let nextColor =
                { new Lens<BoxSelection.BoxSelectionModel, BoxSelection.ColorIndex>() with
                    override x.Get(r) = r.nextColor
                    override x.Set(r,v) = { r with nextColor = v }
                    override x.Update(r,f) = { r with nextColor = f r.nextColor }
                }
