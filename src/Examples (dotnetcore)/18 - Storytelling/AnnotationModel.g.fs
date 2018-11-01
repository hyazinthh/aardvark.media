namespace Annotations

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Annotations

[<AutoOpen>]
module Mutable =

    
    
    type MLabel(__initial : Annotations.Label) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Annotations.Label> = Aardvark.Base.Incremental.EqModRef<Annotations.Label>(__initial) :> Aardvark.Base.Incremental.IModRef<Annotations.Label>
        let _width = ResetMod.Create(__initial.width)
        let _position = ResetMod.Create(__initial.position)
        let _text = ResetMod.Create(__initial.text)
        
        member x.width = _width :> IMod<_>
        member x.position = _position :> IMod<_>
        member x.text = _text :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Annotations.Label) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_width,v.width)
                ResetMod.Update(_position,v.position)
                ResetMod.Update(_text,v.text)
                
        
        static member Create(__initial : Annotations.Label) : MLabel = MLabel(__initial)
        static member Update(m : MLabel, v : Annotations.Label) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Annotations.Label> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Label =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let width =
                { new Lens<Annotations.Label, System.Int32>() with
                    override x.Get(r) = r.width
                    override x.Set(r,v) = { r with width = v }
                    override x.Update(r,f) = { r with width = f r.width }
                }
            let position =
                { new Lens<Annotations.Label, Aardvark.Base.V2i>() with
                    override x.Get(r) = r.position
                    override x.Set(r,v) = { r with position = v }
                    override x.Update(r,f) = { r with position = f r.position }
                }
            let text =
                { new Lens<Annotations.Label, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
    
    
    type MAnnotation(__initial : Annotations.Annotation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Annotations.Annotation> = Aardvark.Base.Incremental.EqModRef<Annotations.Annotation>(__initial) :> Aardvark.Base.Incremental.IModRef<Annotations.Annotation>
        let _id = ResetMod.Create(__initial.id)
        let _target = MOption.Create(__initial.target)
        let _label = MLabel.Create(__initial.label)
        
        member x.id = _id :> IMod<_>
        member x.target = _target :> IMod<_>
        member x.label = _label
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Annotations.Annotation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                MOption.Update(_target, v.target)
                MLabel.Update(_label, v.label)
                
        
        static member Create(__initial : Annotations.Annotation) : MAnnotation = MAnnotation(__initial)
        static member Update(m : MAnnotation, v : Annotations.Annotation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Annotations.Annotation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Annotation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Annotations.Annotation, Annotations.AnnotationId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let target =
                { new Lens<Annotations.Annotation, Microsoft.FSharp.Core.Option<Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.target
                    override x.Set(r,v) = { r with target = v }
                    override x.Update(r,f) = { r with target = f r.target }
                }
            let label =
                { new Lens<Annotations.Annotation, Annotations.Label>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
    
    
    type MAnnotations(__initial : Annotations.Annotations) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Annotations.Annotations> = Aardvark.Base.Incremental.EqModRef<Annotations.Annotations>(__initial) :> Aardvark.Base.Incremental.IModRef<Annotations.Annotations>
        let _list = MList.Create(__initial.list, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _focus = MOption.Create(__initial.focus, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        
        member x.list = _list :> alist<_>
        member x.focus = _focus :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Annotations.Annotations) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_list, v.list)
                MOption.Update(_focus, v.focus)
                
        
        static member Create(__initial : Annotations.Annotations) : MAnnotations = MAnnotations(__initial)
        static member Update(m : MAnnotations, v : Annotations.Annotations) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Annotations.Annotations> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Annotations =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let list =
                { new Lens<Annotations.Annotations, Aardvark.Base.plist<Annotations.Annotation>>() with
                    override x.Get(r) = r.list
                    override x.Set(r,v) = { r with list = v }
                    override x.Update(r,f) = { r with list = f r.list }
                }
            let focus =
                { new Lens<Annotations.Annotations, Microsoft.FSharp.Core.Option<Annotations.Annotation>>() with
                    override x.Get(r) = r.focus
                    override x.Set(r,v) = { r with focus = v }
                    override x.Update(r,f) = { r with focus = f r.focus }
                }
