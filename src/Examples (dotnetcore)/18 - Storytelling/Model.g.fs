namespace Model

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Model

[<AutoOpen>]
module Mutable =

    
    
    type MAnimation(__initial : Model.Animation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Model.Animation> = Aardvark.Base.Incremental.EqModRef<Model.Animation>(__initial) :> Aardvark.Base.Incremental.IModRef<Model.Animation>
        let _model = Aardvark.UI.Animation.Mutable.MAnimationModel.Create(__initial.model)
        let _savedView = ResetMod.Create(__initial.savedView)
        
        member x.model = _model
        member x.savedView = _savedView :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Model.Animation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Animation.Mutable.MAnimationModel.Update(_model, v.model)
                ResetMod.Update(_savedView,v.savedView)
                
        
        static member Create(__initial : Model.Animation) : MAnimation = MAnimation(__initial)
        static member Update(m : MAnimation, v : Model.Animation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Model.Animation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Animation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let model =
                { new Lens<Model.Animation, Aardvark.UI.Animation.AnimationModel>() with
                    override x.Get(r) = r.model
                    override x.Set(r,v) = { r with model = v }
                    override x.Update(r,f) = { r with model = f r.model }
                }
            let savedView =
                { new Lens<Model.Animation, Aardvark.Base.CameraView>() with
                    override x.Get(r) = r.savedView
                    override x.Set(r,v) = { r with savedView = v }
                    override x.Update(r,f) = { r with savedView = f r.savedView }
                }
    
    
    type MModel(__initial : Model.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Model.Model> = Aardvark.Base.Incremental.EqModRef<Model.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<Model.Model>
        let _appModel = BoxSelection.Mutable.MBoxSelectionModel.Create(__initial.appModel)
        let _dockConfig = ResetMod.Create(__initial.dockConfig)
        let _provenance = Provenance.Mutable.MProvenance.Create(__initial.provenance)
        let _story = Story.Mutable.MStory.Create(__initial.story)
        let _presentation = ResetMod.Create(__initial.presentation)
        let _thumbnailRequests = MSet.Create(__initial.thumbnailRequests)
        let _animation = MAnimation.Create(__initial.animation)
        
        member x.appModel = _appModel
        member x.dockConfig = _dockConfig :> IMod<_>
        member x.provenance = _provenance
        member x.story = _story
        member x.presentation = _presentation :> IMod<_>
        member x.thumbnailRequests = _thumbnailRequests :> aset<_>
        member x.animation = _animation
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Model.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                BoxSelection.Mutable.MBoxSelectionModel.Update(_appModel, v.appModel)
                ResetMod.Update(_dockConfig,v.dockConfig)
                Provenance.Mutable.MProvenance.Update(_provenance, v.provenance)
                Story.Mutable.MStory.Update(_story, v.story)
                ResetMod.Update(_presentation,v.presentation)
                MSet.Update(_thumbnailRequests, v.thumbnailRequests)
                MAnimation.Update(_animation, v.animation)
                
        
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
            let presentation =
                { new Lens<Model.Model, System.Boolean>() with
                    override x.Get(r) = r.presentation
                    override x.Set(r,v) = { r with presentation = v }
                    override x.Update(r,f) = { r with presentation = f r.presentation }
                }
            let thumbnailRequests =
                { new Lens<Model.Model, Aardvark.Base.hset<Story.SlideId>>() with
                    override x.Get(r) = r.thumbnailRequests
                    override x.Set(r,v) = { r with thumbnailRequests = v }
                    override x.Update(r,f) = { r with thumbnailRequests = f r.thumbnailRequests }
                }
            let animation =
                { new Lens<Model.Model, Model.Animation>() with
                    override x.Get(r) = r.animation
                    override x.Set(r,v) = { r with animation = v }
                    override x.Update(r,f) = { r with animation = f r.animation }
                }
