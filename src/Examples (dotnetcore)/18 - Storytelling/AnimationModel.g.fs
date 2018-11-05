namespace Animation

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Animation

[<AutoOpen>]
module Mutable =

    
    
    type MAnimation(__initial : Animation.Animation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Animation.Animation> = Aardvark.Base.Incremental.EqModRef<Animation.Animation>(__initial) :> Aardvark.Base.Incremental.IModRef<Animation.Animation>
        let _model = Aardvark.UI.Animation.Mutable.MAnimationModel.Create(__initial.model)
        let _savedView = ResetMod.Create(__initial.savedView)
        
        member x.model = _model
        member x.savedView = _savedView :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Animation.Animation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Animation.Mutable.MAnimationModel.Update(_model, v.model)
                ResetMod.Update(_savedView,v.savedView)
                
        
        static member Create(__initial : Animation.Animation) : MAnimation = MAnimation(__initial)
        static member Update(m : MAnimation, v : Animation.Animation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Animation.Animation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Animation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let model =
                { new Lens<Animation.Animation, Aardvark.UI.Animation.AnimationModel>() with
                    override x.Get(r) = r.model
                    override x.Set(r,v) = { r with model = v }
                    override x.Update(r,f) = { r with model = f r.model }
                }
            let savedView =
                { new Lens<Animation.Animation, Aardvark.Base.CameraView>() with
                    override x.Get(r) = r.savedView
                    override x.Set(r,v) = { r with savedView = v }
                    override x.Update(r,f) = { r with savedView = f r.savedView }
                }
