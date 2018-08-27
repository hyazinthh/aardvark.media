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
        let _dockConfig = ResetMod.Create(__initial.dockConfig)
        let _provenance = Provenance.Mutable.MProvenance.Create(__initial.provenance)
        let _story = Story.Mutable.MStory.Create(__initial.story)
        
        member x.appModel = _appModel
        member x.dockConfig = _dockConfig :> IMod<_>
        member x.provenance = _provenance
        member x.story = _story
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Model.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                BoxSelection.Mutable.MBoxSelectionModel.Update(_appModel, v.appModel)
                ResetMod.Update(_dockConfig,v.dockConfig)
                Provenance.Mutable.MProvenance.Update(_provenance, v.provenance)
                Story.Mutable.MStory.Update(_story, v.story)
                
        
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
            let dockConfig =
                { new Lens<Model.Model, Aardvark.UI.Primitives.DockConfig>() with
                    override x.Get(r) = r.dockConfig
                    override x.Set(r,v) = { r with dockConfig = v }
                    override x.Update(r,f) = { r with dockConfig = f r.dockConfig }
                }
            let provenance =
                { new Lens<Model.Model, Provenance.Provenance>() with
                    override x.Get(r) = r.provenance
                    override x.Set(r,v) = { r with provenance = v }
                    override x.Update(r,f) = { r with provenance = f r.provenance }
                }
            let story =
                { new Lens<Model.Model, Story.Story>() with
                    override x.Get(r) = r.story
                    override x.Set(r,v) = { r with story = v }
                    override x.Update(r,f) = { r with story = f r.story }
                }
