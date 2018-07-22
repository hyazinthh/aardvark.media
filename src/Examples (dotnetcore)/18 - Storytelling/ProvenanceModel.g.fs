namespace Provenance

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Provenance

[<AutoOpen>]
module Mutable =

    
    
    type MState(__initial : Provenance.State) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.State> = Aardvark.Base.Incremental.EqModRef<Provenance.State>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.State>
        let _view = ResetMod.Create(__initial.view)
        let _boxes = MMap.Create(__initial.boxes, (fun v -> BoxSelection.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> BoxSelection.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _nextColor = ResetMod.Create(__initial.nextColor)
        
        member x.view = _view :> IMod<_>
        member x.boxes = _boxes :> amap<_,_>
        member x.nextColor = _nextColor :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.State) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_view,v.view)
                MMap.Update(_boxes, v.boxes)
                ResetMod.Update(_nextColor,v.nextColor)
                
        
        static member Create(__initial : Provenance.State) : MState = MState(__initial)
        static member Update(m : MState, v : Provenance.State) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Provenance.State> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let view =
                { new Lens<Provenance.State, Aardvark.Base.CameraView>() with
                    override x.Get(r) = r.view
                    override x.Set(r,v) = { r with view = v }
                    override x.Update(r,f) = { r with view = f r.view }
                }
            let boxes =
                { new Lens<Provenance.State, Aardvark.Base.hmap<BoxSelection.BoxId,BoxSelection.VisibleBox>>() with
                    override x.Get(r) = r.boxes
                    override x.Set(r,v) = { r with boxes = v }
                    override x.Update(r,f) = { r with boxes = f r.boxes }
                }
            let nextColor =
                { new Lens<Provenance.State, BoxSelection.ColorIndex>() with
                    override x.Get(r) = r.nextColor
                    override x.Set(r,v) = { r with nextColor = v }
                    override x.Update(r,f) = { r with nextColor = f r.nextColor }
                }
    
    
    type MNode(__initial : Provenance.Node) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Node> = Aardvark.Base.Incremental.EqModRef<Provenance.Node>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Node>
        let _id = ResetMod.Create(__initial.id)
        let _state = MState.Create(__initial.state)
        let _message = MOption.Create(__initial.message)
        
        member x.id = _id :> IMod<_>
        member x.state = _state
        member x.message = _message :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Node) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                MState.Update(_state, v.state)
                MOption.Update(_message, v.message)
                
        
        static member Create(__initial : Provenance.Node) : MNode = MNode(__initial)
        static member Update(m : MNode, v : Provenance.Node) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Provenance.Node> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Node =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Provenance.Node, Provenance.NodeId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let state =
                { new Lens<Provenance.Node, Provenance.State>() with
                    override x.Get(r) = r.state
                    override x.Set(r,v) = { r with state = v }
                    override x.Update(r,f) = { r with state = f r.state }
                }
            let message =
                { new Lens<Provenance.Node, Microsoft.FSharp.Core.Option<Provenance.Message>>() with
                    override x.Get(r) = r.message
                    override x.Set(r,v) = { r with message = v }
                    override x.Update(r,f) = { r with message = f r.message }
                }
    
    
    type MProvenance(__initial : Provenance.Provenance) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Provenance> = Aardvark.Base.Incremental.EqModRef<Provenance.Provenance>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Provenance>
        let _tree = ResetMod.Create(__initial.tree)
        let _next = MOption.Create(__initial.next, (fun v -> MProvenance.Create(v)), (fun (m,v) -> MProvenance.Update(m, v)), (fun v -> v))
        
        member x.tree = _tree :> IMod<_>
        member x.next = _next :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Provenance) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_tree,v.tree)
                MOption.Update(_next, v.next)
                
        
        static member Create(__initial : Provenance.Provenance) : MProvenance = MProvenance(__initial)
        static member Update(m : MProvenance, v : Provenance.Provenance) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Provenance.Provenance> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Provenance =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let tree =
                { new Lens<Provenance.Provenance, BoxSelection.tree<Provenance.Node>>() with
                    override x.Get(r) = r.tree
                    override x.Set(r,v) = { r with tree = v }
                    override x.Update(r,f) = { r with tree = f r.tree }
                }
            let next =
                { new Lens<Provenance.Provenance, Microsoft.FSharp.Core.Option<Provenance.Provenance>>() with
                    override x.Get(r) = r.next
                    override x.Set(r,v) = { r with next = v }
                    override x.Update(r,f) = { r with next = f r.next }
                }
