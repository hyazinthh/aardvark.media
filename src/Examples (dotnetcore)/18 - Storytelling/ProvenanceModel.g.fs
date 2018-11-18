namespace Provenance

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Provenance

[<AutoOpen>]
module Mutable =

    
    
    type MNode(__initial : Provenance.Node) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Node> = Aardvark.Base.Incremental.EqModRef<Provenance.Node>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Node>
        let _id = ResetMod.Create(__initial.id)
        let _state = Provenance.Reduced.Mutable.MState.Create(__initial.state)
        let _message = MOption.Create(__initial.message, (fun v -> Provenance.Reduced.Mutable.MMessage.Create(v)), (fun (m,v) -> Provenance.Reduced.Mutable.MMessage.Update(m, v)), (fun v -> v))
        
        member x.id = _id :> IMod<_>
        member x.state = _state
        member x.message = _message :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Node) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                Provenance.Reduced.Mutable.MState.Update(_state, v.state)
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
                { new Lens<Provenance.Node, Provenance.Reduced.State>() with
                    override x.Get(r) = r.state
                    override x.Set(r,v) = { r with state = v }
                    override x.Update(r,f) = { r with state = f r.state }
                }
            let message =
                { new Lens<Provenance.Node, Microsoft.FSharp.Core.Option<Provenance.Reduced.Message>>() with
                    override x.Get(r) = r.message
                    override x.Set(r,v) = { r with message = v }
                    override x.Update(r,f) = { r with message = f r.message }
                }
    
    
    type MProvenance(__initial : Provenance.Provenance) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Provenance.Provenance> = Aardvark.Base.Incremental.EqModRef<Provenance.Provenance>(__initial) :> Aardvark.Base.Incremental.IModRef<Provenance.Provenance>
        let _tree = ResetMod.Create(__initial.tree)
        let _hovered = MOption.Create(__initial.hovered)
        let _highlight = MOption.Create(__initial.highlight)
        
        member x.tree = _tree :> IMod<_>
        member x.hovered = _hovered :> IMod<_>
        member x.highlight = _highlight :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Provenance.Provenance) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_tree,v.tree)
                MOption.Update(_hovered, v.hovered)
                MOption.Update(_highlight, v.highlight)
                
        
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
                { new Lens<Provenance.Provenance, Aardvark.Base.ZTree<Provenance.Node>>() with
                    override x.Get(r) = r.tree
                    override x.Set(r,v) = { r with tree = v }
                    override x.Update(r,f) = { r with tree = f r.tree }
                }
            let hovered =
                { new Lens<Provenance.Provenance, Microsoft.FSharp.Core.Option<Provenance.NodeId>>() with
                    override x.Get(r) = r.hovered
                    override x.Set(r,v) = { r with hovered = v }
                    override x.Update(r,f) = { r with hovered = f r.hovered }
                }
            let highlight =
                { new Lens<Provenance.Provenance, Microsoft.FSharp.Core.Option<Provenance.NodeId>>() with
                    override x.Get(r) = r.highlight
                    override x.Set(r,v) = { r with highlight = v }
                    override x.Update(r,f) = { r with highlight = f r.highlight }
                }
