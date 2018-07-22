namespace Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Model

[<AutoOpen>]
module Mutable =

    
    
    type MModel(__initial : Model.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Model.Model> = Aardvark.Base.Incremental.EqModRef<Model.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<Model.Model>
        let _appModel = BoxSelection.Mutable.MBoxSelectionModel.Create(__initial.appModel)
        let _provenance = Provenance.Mutable.MProvenance.Create(__initial.provenance)
        
        member x.appModel = _appModel
        member x.provenance = _provenance
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Model.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                BoxSelection.Mutable.MBoxSelectionModel.Update(_appModel, v.appModel)
                Provenance.Mutable.MProvenance.Update(_provenance, v.provenance)
                
        
        static member Create(__initial : Model.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : Model.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Model.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let appModel =
                { new Lens<Model.Model, BoxSelection.BoxSelectionModel>() with
                    override x.Get(r) = r.appModel
                    override x.Set(r,v) = { r with appModel = v }
                    override x.Update(r,f) = { r with appModel = f r.appModel }
                }
            let provenance =
                { new Lens<Model.Model, Provenance.Provenance>() with
                    override x.Get(r) = r.provenance
                    override x.Set(r,v) = { r with provenance = v }
                    override x.Update(r,f) = { r with provenance = f r.provenance }
                }
