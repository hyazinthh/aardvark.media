namespace Provenance.Reduced

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Provenance.Reduced

[<AutoOpen>]
module Mutable =

    
    
    type MCameraView(__initial : Provenance.Reduced.CameraView) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Reduced.CameraView> = Aardvark.Base.Incremental.EqModRef<Provenance.Reduced.CameraView>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Reduced.CameraView>
        let _sky = ResetMod.Create(__initial.sky)
        let _location = ResetMod.Create(__initial.location)
        let _forward = ResetMod.Create(__initial.forward)
        let _up = ResetMod.Create(__initial.up)
        let _right = ResetMod.Create(__initial.right)
        
        member x.sky = _sky :> IMod<_>
        member x.location = _location :> IMod<_>
        member x.forward = _forward :> IMod<_>
        member x.up = _up :> IMod<_>
        member x.right = _right :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Reduced.CameraView) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_sky,v.sky)
                ResetMod.Update(_location,v.location)
                ResetMod.Update(_forward,v.forward)
                ResetMod.Update(_up,v.up)
                ResetMod.Update(_right,v.right)
                
        
        static member Create(__initial : Provenance.Reduced.CameraView) : MCameraView = MCameraView(__initial)
        static member Update(m : MCameraView, v : Provenance.Reduced.CameraView) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Provenance.Reduced.CameraView> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CameraView =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let sky =
                { new Lens<Provenance.Reduced.CameraView, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.sky
                    override x.Set(r,v) = { r with sky = v }
                    override x.Update(r,f) = { r with sky = f r.sky }
                }
            let location =
                { new Lens<Provenance.Reduced.CameraView, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.location
                    override x.Set(r,v) = { r with location = v }
                    override x.Update(r,f) = { r with location = f r.location }
                }
            let forward =
                { new Lens<Provenance.Reduced.CameraView, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.forward
                    override x.Set(r,v) = { r with forward = v }
                    override x.Update(r,f) = { r with forward = f r.forward }
                }
            let up =
                { new Lens<Provenance.Reduced.CameraView, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.up
                    override x.Set(r,v) = { r with up = v }
                    override x.Update(r,f) = { r with up = f r.up }
                }
            let right =
                { new Lens<Provenance.Reduced.CameraView, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.right
                    override x.Set(r,v) = { r with right = v }
                    override x.Update(r,f) = { r with right = f r.right }
                }
    [<AbstractClass; System.Runtime.CompilerServices.Extension; StructuredFormatDisplay("{AsString}")>]
    type MMessage() =
        abstract member TryUpdate : Provenance.Reduced.Message -> bool
        abstract member AsString : string
        
        static member private CreateValue(__model : Provenance.Reduced.Message) = 
            match __model with
                | Scale(item) -> MScale(__model, item) :> MMessage
                | Rotate(item) -> MRotate(__model, item) :> MMessage
                | Translate(item) -> MTranslate(__model, item) :> MMessage
                | AddBox -> MAddBox(__model) :> MMessage
                | RemoveBox -> MRemoveBox(__model) :> MMessage
                | Unknown -> MUnknown(__model) :> MMessage
        
        static member Create(v : Provenance.Reduced.Message) =
            ResetMod.Create(MMessage.CreateValue v) :> IMod<_>
        
        [<System.Runtime.CompilerServices.Extension>]
        static member Update(m : IMod<MMessage>, v : Provenance.Reduced.Message) =
            let m = unbox<ResetMod<MMessage>> m
            if not (m.GetValue().TryUpdate v) then
                m.Update(MMessage.CreateValue v)
    
    and private MScale(__initial : Provenance.Reduced.Message, item : BoxSelection.BoxId) =
        inherit MMessage()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Provenance.Reduced.Message) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Scale(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    and private MRotate(__initial : Provenance.Reduced.Message, item : BoxSelection.BoxId) =
        inherit MMessage()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Provenance.Reduced.Message) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Rotate(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    and private MTranslate(__initial : Provenance.Reduced.Message, item : BoxSelection.BoxId) =
        inherit MMessage()
        
        let mutable __current = __initial
        let _item = ResetMod.Create(item)
        member x.item = _item :> IMod<_>
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Provenance.Reduced.Message) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Translate(item) -> 
                        __current <- __model
                        _item.Update(item)
                        true
                    | _ -> false
    
    and private MAddBox(__initial : Provenance.Reduced.Message) =
        inherit MMessage()
        
        let mutable __current = __initial
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Provenance.Reduced.Message) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | AddBox -> 
                        __current <- __model
                        true
                    | _ -> false
    
    and private MRemoveBox(__initial : Provenance.Reduced.Message) =
        inherit MMessage()
        
        let mutable __current = __initial
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Provenance.Reduced.Message) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | RemoveBox -> 
                        __current <- __model
                        true
                    | _ -> false
    
    and private MUnknown(__initial : Provenance.Reduced.Message) =
        inherit MMessage()
        
        let mutable __current = __initial
        
        override x.ToString() = __current.ToString()
        override x.AsString = sprintf "%A" __current
        
        override x.TryUpdate(__model : Provenance.Reduced.Message) = 
            if System.Object.ReferenceEquals(__current, __model) then
                true
            else
                match __model with
                    | Unknown -> 
                        __current <- __model
                        true
                    | _ -> false
    
    
    [<AutoOpen>]
    module MMessagePatterns =
        let (|MScale|MRotate|MTranslate|MAddBox|MRemoveBox|MUnknown|) (m : MMessage) =
            match m with
            | :? MScale as v -> MScale(v.item)
            | :? MRotate as v -> MRotate(v.item)
            | :? MTranslate as v -> MTranslate(v.item)
            | :? MAddBox as v -> MAddBox
            | :? MRemoveBox as v -> MRemoveBox
            | :? MUnknown as v -> MUnknown
            | _ -> failwith "impossible"
    
    
    
    
    
    
    type MTransformation(__initial : Provenance.Reduced.Transformation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Reduced.Transformation> = Aardvark.Base.Incremental.EqModRef<Provenance.Reduced.Transformation>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Reduced.Transformation>
        let _pose = ResetMod.Create(__initial.pose)
        
        member x.pose = _pose :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Reduced.Transformation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_pose,v.pose)
                
        
        static member Create(__initial : Provenance.Reduced.Transformation) : MTransformation = MTransformation(__initial)
        static member Update(m : MTransformation, v : Provenance.Reduced.Transformation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Provenance.Reduced.Transformation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Transformation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let pose =
                { new Lens<Provenance.Reduced.Transformation, Aardvark.UI.Trafos.Pose>() with
                    override x.Get(r) = r.pose
                    override x.Set(r,v) = { r with pose = v }
                    override x.Update(r,f) = { r with pose = f r.pose }
                }
    
    
    type MVisibleBox(__initial : Provenance.Reduced.VisibleBox) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Reduced.VisibleBox> = Aardvark.Base.Incremental.EqModRef<Provenance.Reduced.VisibleBox>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Reduced.VisibleBox>
        let _geometry = ResetMod.Create(__initial.geometry)
        let _color = ResetMod.Create(__initial.color)
        let _transform = MTransformation.Create(__initial.transform)
        
        member x.geometry = _geometry :> IMod<_>
        member x.color = _color :> IMod<_>
        member x.transform = _transform
        member x.id = __current.Value.id
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Reduced.VisibleBox) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_geometry,v.geometry)
                ResetMod.Update(_color,v.color)
                MTransformation.Update(_transform, v.transform)
                
        
        static member Create(__initial : Provenance.Reduced.VisibleBox) : MVisibleBox = MVisibleBox(__initial)
        static member Update(m : MVisibleBox, v : Provenance.Reduced.VisibleBox) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Provenance.Reduced.VisibleBox> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module VisibleBox =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let geometry =
                { new Lens<Provenance.Reduced.VisibleBox, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let color =
                { new Lens<Provenance.Reduced.VisibleBox, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let transform =
                { new Lens<Provenance.Reduced.VisibleBox, Provenance.Reduced.Transformation>() with
                    override x.Get(r) = r.transform
                    override x.Set(r,v) = { r with transform = v }
                    override x.Update(r,f) = { r with transform = f r.transform }
                }
            let id =
                { new Lens<Provenance.Reduced.VisibleBox, BoxSelection.BoxId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
    
    
    type MState(__initial : Provenance.Reduced.State) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Reduced.State> = Aardvark.Base.Incremental.EqModRef<Provenance.Reduced.State>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Reduced.State>
        let _boxes = MMap.Create(__initial.boxes, (fun v -> MVisibleBox.Create(v)), (fun (m,v) -> MVisibleBox.Update(m, v)), (fun v -> v))
        let _nextColor = ResetMod.Create(__initial.nextColor)
        
        member x.boxes = _boxes :> amap<_,_>
        member x.nextColor = _nextColor :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Reduced.State) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_boxes, v.boxes)
                ResetMod.Update(_nextColor,v.nextColor)
                
        
        static member Create(__initial : Provenance.Reduced.State) : MState = MState(__initial)
        static member Update(m : MState, v : Provenance.Reduced.State) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Provenance.Reduced.State> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let boxes =
                { new Lens<Provenance.Reduced.State, Aardvark.Base.hmap<BoxSelection.BoxId,Provenance.Reduced.VisibleBox>>() with
                    override x.Get(r) = r.boxes
                    override x.Set(r,v) = { r with boxes = v }
                    override x.Update(r,f) = { r with boxes = f r.boxes }
                }
            let nextColor =
                { new Lens<Provenance.Reduced.State, BoxSelection.ColorIndex>() with
                    override x.Get(r) = r.nextColor
                    override x.Set(r,v) = { r with nextColor = v }
                    override x.Update(r,f) = { r with nextColor = f r.nextColor }
                }
